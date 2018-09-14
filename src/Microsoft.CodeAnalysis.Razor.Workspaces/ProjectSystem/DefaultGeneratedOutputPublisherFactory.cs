// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using Microsoft.CodeAnalysis.Host;
using Microsoft.CodeAnalysis.Host.Mef;

namespace Microsoft.CodeAnalysis.Razor.ProjectSystem
{
    [ExportWorkspaceServiceFactory(typeof(GeneratedOutputPublisher), ServiceLayer.Default)]
    public class DefaultGeneratedOutputPublisherFactory : IWorkspaceServiceFactory
    {
        public IWorkspaceService CreateService(HostWorkspaceServices workspaceServices)
        {
            return new DefaultGeneratedOutputPublisher();
        }
    }
}
