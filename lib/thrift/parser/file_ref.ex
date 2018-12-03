defmodule Thrift.Parser.FileRef do
  @moduledoc false

  @type t :: %__MODULE__{path: Path.t(), contents: String.t()}
  defstruct path: nil, contents: nil

  def new(path) do
    # We include the __file__ here to hack around the fact that leex and yecc don't
    # operate on files and lose the file info. This is relevant because the filename is
    # turned into the thrift module, and is necessary for resolution.
    thrift_file = File.read!(path) <> "\n__file__ \"#{path}\""
    %__MODULE__{path: path, contents: thrift_file}
  end
end
