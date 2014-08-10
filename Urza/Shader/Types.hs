module Urza.Shader.Types where

import Graphics.Rendering.OpenGL

type VertexBuffer a = (BufferObject, VertexArrayDescriptor a, AttribLocation)

