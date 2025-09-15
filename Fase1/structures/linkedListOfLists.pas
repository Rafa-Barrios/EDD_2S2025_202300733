{$MODE DELPHI}

unit linkedListOfLists;

interface

uses
    SysUtils, Classes, simpleLinkedList; // 👈 Importamos la lista simple para validar usuarios

type
    // Datos de una celda
    TCellData = record
        name: string;
    end;

    // Datos de una cabecera
    THeaderData = record
        name: string;
    end;

    // Funciones públicas
    procedure LL_InsertHeader(name: string);
    procedure LL_InsertElement(headerName, cellName: string);
    function LL_GenerateDot: string;
    procedure LL_Clear;

implementation

type
    // Nodo Cell (celda)
    PCell = ^TCell;
    TCell = record
        name: string;
        next: PCell;
    end;

    // Nodo Header (cabecera)
    PHeader = ^THeader;
    THeader = record
        name: string;
        cells: PCell;  // lista de celdas
        next: PHeader;
    end;

var
    HeaderList: PHeader = nil;

    // Escapar caracteres especiales para DOT
    function EscapeDotString(const S: string): string;
    var
        Res: string;
        i: Integer;
    begin
        Res := '';
        for i := 1 to Length(S) do
        begin
            case S[i] of
                '"': Res := Res + '\"';
                '\': Res := Res + '\\';
                '|': Res := Res + '\|';
                '{': Res := Res + '\{';
                '}': Res := Res + '\}';
                #10, #13: Res := Res + '\n';
            else
                Res := Res + S[i];
            end;
        end;
        Result := Res;
    end;

    // Insertar cabecera
    procedure LL_InsertHeader(name: string);
    var
        newHeader, current: PHeader;
    begin
        New(newHeader);
        newHeader^.name := Trim(name);
        newHeader^.cells := nil;
        newHeader^.next := nil;

        if HeaderList = nil then
            HeaderList := newHeader
        else
        begin
            // Validar que no exista
            current := HeaderList;
            while current <> nil do
            begin
                if SameText(current^.name, newHeader^.name) then
                begin
                    Writeln('Error: La cabecera "', name, '" ya existe.');
                    Dispose(newHeader);
                    Exit;
                end;
                current := current^.next;
            end;

            // Insertar al final
            current := HeaderList;
            while current^.next <> nil do
                current := current^.next;
            current^.next := newHeader;
        end;
    end;

    // Insertar celda en cabecera
    procedure LL_InsertElement(headerName, cellName: string);
    var
        currentHeader: PHeader;
        newCell, currentCell: PCell;
    begin
        currentHeader := HeaderList;
        while (currentHeader <> nil) and (currentHeader^.name <> Trim(headerName)) do
            currentHeader := currentHeader^.next;

        if currentHeader = nil then
        begin
            Writeln('Error: No se encontró la cabecera ', headerName);
            Exit;
        end;

        // 🔍 Verificar si el usuario existe en la lista simple
        if not LSL_UserExists(Trim(cellName)) then
        begin
            Writeln('Error: El usuario "', cellName, '" no existe en el sistema.');
            Exit;
        end;

        // Crear nueva celda
        New(newCell);
        newCell^.name := Trim(cellName);
        newCell^.next := nil;

        if currentHeader^.cells = nil then
            currentHeader^.cells := newCell
        else
        begin
            currentCell := currentHeader^.cells;
            while currentCell^.next <> nil do
                currentCell := currentCell^.next;
            currentCell^.next := newCell;
        end;
    end;

    // Generar DOT
    function LL_GenerateDot: string;
    var
        SL: TStringList;
        currentHeader: PHeader;
        currentCell: PCell;
        headerCounter, cellCounter: Integer;
        headerNode, cellNode, nextNode: string;
        rankLine: string;
    begin
        SL := TStringList.Create;

        SL.Add('digraph ListaDeListas {');
        SL.Add('  rankdir=LR;');
        SL.Add('  nodesep=0.6;');
        SL.Add('  edge [arrowhead=normal];');
        SL.Add('  node [shape=box, style=filled, fillcolor=lightyellow];');
        SL.Add('');

        if HeaderList = nil then
            SL.Add('  null [label="VACÍA", shape=plaintext];')
        else
        begin
            headerCounter := 0;
            currentHeader := HeaderList;
            while currentHeader <> nil do
            begin
                headerNode := Format('header%d', [headerCounter]);
                SL.Add(Format('  %s [label="%s", fillcolor=lightblue];',
                    [headerNode, EscapeDotString(currentHeader^.name)]));

                // Conexión entre cabeceras
                if currentHeader^.next <> nil then
                begin
                    nextNode := Format('header%d', [headerCounter + 1]);
                    SL.Add(Format('  %s -> %s;', [headerNode, nextNode]));
                end;

                // Recorrer celdas
                if currentHeader^.cells <> nil then
                begin
                    rankLine := '{ rank=same; ' + headerNode;
                    cellCounter := 0;
                    currentCell := currentHeader^.cells;

                    // Conectar cabecera a primera celda
                    cellNode := Format('cell%d_%d', [headerCounter, cellCounter]);
                    SL.Add(Format('  %s -> %s;', [headerNode, cellNode]));

                    while currentCell <> nil do
                    begin
                        cellNode := Format('cell%d_%d', [headerCounter, cellCounter]);
                        SL.Add(Format('  %s [label="Usuario: %s"];',
                            [cellNode, EscapeDotString(currentCell^.name)]));

                        rankLine := rankLine + ' ' + cellNode;

                        // Conexión entre celdas
                        if currentCell^.next <> nil then
                        begin
                            nextNode := Format('cell%d_%d', [headerCounter, cellCounter + 1]);
                            SL.Add(Format('  %s -> %s;', [cellNode, nextNode]));
                        end;

                        Inc(cellCounter);
                        currentCell := currentCell^.next;
                    end;

                    rankLine := rankLine + ' }';
                    SL.Add(rankLine);
                end;

                Inc(headerCounter);
                currentHeader := currentHeader^.next;
            end;
        end;

        SL.Add('}');
        Result := SL.Text;
        SL.Free;
    end;

    // Liberar memoria
    procedure LL_Clear;
    var
        currentHeader, tempHeader: PHeader;
        currentCell, tempCell: PCell;
    begin
        currentHeader := HeaderList;
        while currentHeader <> nil do
        begin
            currentCell := currentHeader^.cells;
            while currentCell <> nil do
            begin
                tempCell := currentCell;
                currentCell := currentCell^.next;
                Dispose(tempCell);
            end;
            tempHeader := currentHeader;
            currentHeader := currentHeader^.next;
            Dispose(tempHeader);
        end;
        HeaderList := nil;
    end;

end.

