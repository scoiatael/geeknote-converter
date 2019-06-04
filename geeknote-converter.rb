class GeeknoteConverter < Formula
  desc "A helper for https://github.com/VitaliyRodnenko/geeknote"
  homepage "https://github.com/scoiatael/geeknote-converter"
  url "https://github.com/scoiatael/geeknote-converter/releases/download/v0.1.0.1/geeknote-converter-v0.1.0.1-osx.tar.gz"
  sha256 "9478877c8dc7a9a976a97635e6e506fa468d702eec6d90cc4983418d9eb7e94f"

  bottle :unneeded

  def install
    bin.install "geeknote-converter"
  end

  test do
    system "#{bin}/geeknote-converter --help"
  end
end
