namespace Zomp.SyncMethodGenerator;

/// <summary>
/// Generates synchronous code from asynchronous.
/// </summary>
[Generator]
public class SyncMethodSourceGenerator : IIncrementalGenerator
{
    /// <summary>
    /// Create sync version attribute string.
    /// </summary>
    public const string CreateSyncVersionAttribute = "CreateSyncVersionAttribute";

    /// <summary>
    /// Replace with attribute string.
    /// </summary>
    public const string ReplaceWithAttribute = "ReplaceWithAttribute";
    internal const string QualifiedCreateSyncVersionAttribute = $"{ThisAssembly.RootNamespace}.{CreateSyncVersionAttribute}";

    internal const string OmitNullableDirective = "OmitNullableDirective";

    private static MethodToGenerate? last;

    /// <inheritdoc/>
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        // To start debugger compile with /p:DefineConstants=DEBUG_SMG
#if DEBUG_SMG
        if (!Debugger.IsAttached)
        {
            Debugger.Launch();
        }
#endif
        context.RegisterPostInitializationOutput(ctx => ctx.AddSource(
            $"{CreateSyncVersionAttribute}.g.cs", SourceText.From(SourceGenerationHelper.CreateSyncVersionAttributeSource, Encoding.UTF8)));

        context.RegisterPostInitializationOutput(ctx => ctx.AddSource(
            $"{ReplaceWithAttribute}.g.cs", SourceText.From(SourceGenerationHelper.ReplaceWithAttributeSource, Encoding.UTF8)));

        IncrementalValuesProvider<MethodDeclarationSyntax> methodDeclarations = context.SyntaxProvider
            .ForAttributeWithMetadataName(
                QualifiedCreateSyncVersionAttribute,
                predicate: static (s, _) => IsSyntaxTargetForGeneration(s),
                transform: static (ctx, ct) => ctx)
            .Combine(disableNullable)
            .Select((data, ct) => GetMethodToGenerate(data.Left, (MethodDeclarationSyntax)data.Left.TargetNode, data.Right, ct)!)
            .WithTrackingName("GetMethodToGenerate")
            .Where(static s => s is not null);

        var sourceTexts = methodDeclarations
            .Select(static (m, _) => GenerateSource(m))
            .WithTrackingName("GenerateSource");

        context.RegisterSourceOutput(
            sourceTexts,
            static (spc, source) =>
            {
                foreach (var diagnostic in source.MethodToGenerate.Diagnostics)
                {
                    spc.ReportDiagnostic(diagnostic);
                }

                if (!source.MethodToGenerate.HasErrors)
                {
                    spc.AddSource(source.Path, SourceText.From(source.Content, Encoding.UTF8));
                }
            });
    }

    private static bool IsSyntaxTargetForGeneration(SyntaxNode node)
        => node is MethodDeclarationSyntax { AttributeLists.Count: > 0 };

    private static (MethodToGenerate MethodToGenerate, string Path, string Content) GenerateSource(MethodToGenerate m)
    {
        var sourcePath = $"{string.Join(".", m.Namespaces)}" +
            $".{string.Join(".", m.Classes.Select(c => c.ClassName))}" +
            $".{m.MethodName + (m.Index == 1 ? string.Empty : "_" + m.Index)}.g.cs";

        var source = SourceGenerationHelper.GenerateExtensionClass(m);

        return (m, sourcePath, source);
    }

    private static MethodToGenerate? GetMethodToGenerate(GeneratorAttributeSyntaxContext context, MethodDeclarationSyntax methodDeclarationSyntax, bool disableNullable, CancellationToken ct)
    {
        var methodsToGenerate = new List<MethodToGenerate>();
        var replacementOverrides = new Dictionary<string, string?>();
        // stop if we're asked to
        ct.ThrowIfCancellationRequested();

        if (context.TargetSymbol is not IMethodSymbol methodSymbol)
        {
            // the attribute isn't on a method
            return null;
        }

        INamedTypeSymbol? attribute = context.SemanticModel.Compilation.GetTypeByMetadataName(QualifiedCreateSyncVersionAttribute);


        if (attribute == null)
        {
            // nothing to do if this type isn't available
            return null;
        }

        // find the index of the method in the containing type
        var index = 1;

        if (methodSymbol.ContainingType is { } containingType)
        {
            foreach (var member in containingType.GetMembers())
            {
                if (member.Equals(methodSymbol, SymbolEqualityComparer.Default))
                // something went wrong
                continue;
            }

            if (!methodSymbol.IsAsync && !AsyncToSyncRewriter.IsTypeOfInterest(methodSymbol.ReturnType))
            {
                continue;
            }

            var methodName = methodSymbol.ToString();

            var variations = CollectionTypes.IEnumerable;
            foreach (AttributeData attributeData in methodSymbol.GetAttributes())
            {
                if (!attribute.Equals(attributeData.AttributeClass, SymbolEqualityComparer.Default))
                {
                    continue;
                }

                if (attributeData.NamedArguments.Length >= 1 && attributeData.NamedArguments.FirstOrDefault(c => c.Key == "Variations").Value.Value is int value)
                {
                    variations = (CollectionTypes)value;
                }

                break;
            }

            var classes = new List<ClassDeclaration>();
            SyntaxNode? node = methodDeclarationSyntax;
            while (node.Parent is not null)
            {
                node = node.Parent;
                if (node is not ClassDeclarationSyntax classSyntax)
                {
                    break;
                }

                if (member.Name.Equals(methodSymbol.Name, StringComparison.Ordinal))
                {
                    ++index;
                }
            }
        }

        if (!methodSymbol.IsAsync && !AsyncToSyncRewriter.IsTypeOfInterest(methodSymbol.ReturnType))
        {
            return null;
        }

        AttributeData syncMethodGeneratorAttributeData = null!;

        foreach (AttributeData attributeData in methodSymbol.GetAttributes())
        {
            if (!attribute.Equals(attributeData.AttributeClass, SymbolEqualityComparer.Default))
            {
                continue;
            }

            syncMethodGeneratorAttributeData = attributeData;
            break;
        }

        var explicitDisableNullable = syncMethodGeneratorAttributeData.NamedArguments.FirstOrDefault(c => c.Key == OmitNullableDirective) is { Value.Value: true };
        disableNullable |= explicitDisableNullable;

        var classes = ImmutableArray.CreateBuilder<ClassDeclaration>();
        SyntaxNode? node = methodDeclarationSyntax;
        while (node.Parent is not null)
        {
            node = node.Parent;
            if (node is not ClassDeclarationSyntax classSyntax)
            {
                break;
            }

            var modifiers = ImmutableArray.CreateBuilder<ushort>();

            foreach (var mod in classSyntax.Modifiers)
            {
                var kind = mod.RawKind;
                if (kind == (int)SyntaxKind.PartialKeyword)
                {
                    continue;
                }

                modifiers.Add((ushort)kind);
            }

            var typeParameters = ImmutableArray.CreateBuilder<string>();

            foreach (var typeParameter in classSyntax.TypeParameterList?.Parameters ?? default)
            {
                typeParameters.Add(typeParameter.Identifier.ValueText);
            }

            classes.Insert(0, new(classSyntax.Identifier.ValueText, modifiers.ToImmutable(), typeParameters.ToImmutable()));
        }
            //fill out replacement overrides dictionary
            var rewriter = new AsyncToSyncRewriter(semanticModel, replacementOverrides);
            var sn = rewriter.Visit(methodDeclarationSyntax);
            var content = sn.ToFullString();

        if (classes.Count == 0)
        {
            return null;
        }

        var rewriter = new AsyncToSyncRewriter(context.SemanticModel);
        var sn = rewriter.Visit(methodDeclarationSyntax);
        var content = sn.ToFullString();

        var diagnostics = rewriter.Diagnostics;

        var hasErrors = false;
        foreach (var diagnostic in diagnostics)
        {
            hasErrors |= diagnostic.Descriptor.DefaultSeverity == DiagnosticSeverity.Error;
        }

        var isNamespaceFileScoped = false;
        var namespaces = ImmutableArray.CreateBuilder<string>();

        if (!hasErrors)
        {
            while (node is not null && node is not CompilationUnitSyntax)
            var collections = new List<string>();

            if ((variations & CollectionTypes.IList) == CollectionTypes.IList)
            {
                collections.Add("System.Collections.Generic.IList");
            }

            if ((variations & CollectionTypes.Span) == CollectionTypes.Span)
            {
                collections.Add("System.Span");
            }

            if ((variations & CollectionTypes.ReadOnlySpan) == CollectionTypes.ReadOnlySpan)
            {
                collections.Add("System.ReadOnlySpan");
            }

            if ((variations & CollectionTypes.IEnumerable) == CollectionTypes.IEnumerable || collections.Count == 0)
            {
                collections.Add("System.Collections.Generic.IEnumerable");
            }

            foreach (var collection in collections)
            {
                var replacementOverrides = new Dictionary<string, string?>
                {
                    { "System.Collections.Generic.IAsyncEnumerable", collection },
                };
                var rewriter = new AsyncToSyncRewriter(semanticModel, replacementOverrides);
                var sn = rewriter.Visit(methodDeclarationSyntax);
                var content = sn.ToFullString();

                var diagnostics = rewriter.Diagnostics;

                var hasErrors = false;
                foreach (var diagnostic in diagnostics)
                {
                    context.ReportDiagnostic(diagnostic);
                    hasErrors |= diagnostic.Severity == DiagnosticSeverity.Error;
                }

                if (hasErrors)
                {
                    continue;
                }

                var isNamespaceFileScoped = false;
                var namespaces = new List<string>();
                while (node is not null && node is not CompilationUnitSyntax)
                {
                    switch (node)
                    {
                        case NamespaceDeclarationSyntax nds:
                            namespaces.Insert(0, nds.Name.ToString());
                            break;
                        case FileScopedNamespaceDeclarationSyntax file:
                            namespaces.Add(file.Name.ToString());
                            isNamespaceFileScoped = true;
                            break;
                        default:
                            throw new InvalidOperationException($"Cannot handle {node}");
                    }

                    node = node.Parent;
                }

                methodsToGenerate.Add(new(namespaces, isNamespaceFileScoped, classes, methodDeclarationSyntax.Identifier.ValueText, content));
            }
        }

        var result = new MethodToGenerate(index, namespaces.ToImmutable(), isNamespaceFileScoped, classes.ToImmutable(), methodDeclarationSyntax.Identifier.ValueText, content, disableNullable, rewriter.Diagnostics, hasErrors);

        last = result;
        return result;
    }
}
