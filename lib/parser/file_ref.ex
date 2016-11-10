defmodule Thrift.Parser.FileRef do
  @type thrift_include :: String.t
  @type t :: %__MODULE__{path: String.t, include_name: String.t, contents: String.t}
  defstruct path: nil, include_name: nil, contents: nil

  def new(path) do
    thrift_file = File.read!(path) <> "\n__file__ \"#{path}\""
    %__MODULE__{path: path, include_name: include_name(path), contents: thrift_file}
  end

  def include_name(path) do
    Path.basename(path, ".thrift")
  end
end
