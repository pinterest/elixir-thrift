defmodule Thrift.Parser.FileRef do
  @moduledoc false

  @type thrift_include :: String.t
  @type t :: %__MODULE__{path: String.t, include_name: String.t, contents: String.t}
  defstruct path: nil, include_name: nil, contents: nil

  def new(path) do
    # We include the __file__ here to hack around the fact that leex and yecc don't
    # operate on files and lose the file info. This is relevant because the filename is
    # turned into the thrift module, and is necessary for resolution.
    thrift_file = File.read!(path) <> "\n__file__ \"#{path}\""
    %__MODULE__{path: path, include_name: include_name(path), contents: thrift_file}
  end

  def include_name(path) do
    Path.basename(path, ".thrift")
  end
end
