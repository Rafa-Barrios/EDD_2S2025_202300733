unit lzwTools;

{$MODE DELPHI}

interface

uses
  SysUtils, Classes;

// -------------------------------------------------------------
// Función principal: comprimir texto y guardarlo en un archivo .txt
// -------------------------------------------------------------
procedure CompressMessageToFile(const messageText, outputPath: string);

// (Opcional: si más adelante quisieras descomprimir, se puede agregar aquí)
// function DecompressLZW(const inputPath: string): string;

implementation

// -------------------------------------------------------------
// Compresión LZW básica para texto ASCII
// -------------------------------------------------------------
function LZWCompress(const input: string): TBytes;
var
  dict: TStringList;
  data, w, wc: string;
  k: Char;
  i: Integer;
  code: Integer;
  output: TMemoryStream;
begin
  dict := TStringList.Create;
  dict.Sorted := False;
  dict.Duplicates := dupIgnore;

  // Inicializar diccionario con todos los caracteres posibles
  for i := 0 to 255 do
    dict.Add(Char(i));

  output := TMemoryStream.Create;
  w := '';
  for i := 1 to Length(input) do
  begin
    k := input[i];
    wc := w + k;
    if dict.IndexOf(wc) <> -1 then
      w := wc
    else
    begin
      code := dict.IndexOf(w);
      output.WriteBuffer(code, SizeOf(code));
      dict.Add(wc);
      w := k;
    end;
  end;

  if w <> '' then
  begin
    code := dict.IndexOf(w);
    output.WriteBuffer(code, SizeOf(code));
  end;

  SetLength(Result, output.Size);
  output.Position := 0;
  output.ReadBuffer(Result[0], output.Size);

  dict.Free;
  output.Free;
end;

// -------------------------------------------------------------
// Guardar mensaje comprimido en un archivo .txt
// -------------------------------------------------------------
procedure CompressMessageToFile(const messageText, outputPath: string);
var
  compressed: TBytes;
  fs: TFileStream;
begin
  try
    compressed := LZWCompress(messageText);
    fs := TFileStream.Create(outputPath, fmCreate);
    fs.WriteBuffer(compressed[0], Length(compressed));
    fs.Free;
    Writeln('Archivo comprimido creado: ', outputPath);
  except
    on E: Exception do
      Writeln('Error al comprimir y guardar: ', E.Message);
  end;
end;

end.
