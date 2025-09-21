unit jsonLoader;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpjson, jsonparser, bst, Classes;

procedure LoadFromJSON(const FileName: String; var Root: PNode);

implementation

procedure LoadFromJSON(const FileName: String; var Root: PNode);
var
  JSONData: TJSONData;
  JSONArray: TJSONArray;
  Obj: TJSONObject;
  i: Integer;
  FileStream: TFileStream;
begin
  // Abrimos el archivo y lo parseamos
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    JSONData := TJSONParser.Create(FileStream).Parse;
    try
      JSONArray := TJSONArray(JSONData);

      for i := 0 to JSONArray.Count - 1 do
      begin
        Obj := JSONArray.Objects[i];
        InsertNode(Root,
          Obj.Get('id', 0),
          Obj.Get('first_name', ''),
          Obj.Get('last_name', ''),
          Obj.Get('email', ''));
      end;

    finally
      JSONData.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

end.

