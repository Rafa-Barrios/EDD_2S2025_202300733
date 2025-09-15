unit jsonTools;

{$mode objfpc}{$H+}

interface

uses
  simpleLinkedList,
  doubleLinkedList,
  circularLinkedList;

function UploadUsersFromJson(const filePath: string): Boolean;
function AddUserToJson(const filePath: string; const id: Integer; const name, email, username, phone, password: string): Boolean;
function UpdateUserInJson(const filePath: string; const email, username, phone: string): Boolean;

function AddContactToJson(const filePath, propietario, nombre, usuario, correo, telefono: string): Boolean;
function UploadContactsFromJson(const filePath, propietario: string; var contactList: PCNode): Boolean;

function AddMailToJson(const filePath, senderEmail, receiverEmail, subject, body: string): Boolean;
procedure LoadInboxFromJson(const filePath, userEmail: string);
procedure LoadTrashFromJson(const filePath, userEmail: string);
function MoveMailToTrash(const inboxPath, trashPath, userEmail, subject: string): Boolean;

function SaveScheduledToJson(const filePath, userEmail: string): Boolean;
function LoadScheduledFromJson(const filePath, userEmail: string): Boolean;
function SendNextScheduledMail(const senderEmail: string): Boolean;
function RemoveScheduledMailFromJson(const filePath, mailId: string): Boolean;

implementation

uses
  Classes, SysUtils, fpjson, jsonparser, pila, cola;

//
// ---------------------------
// Función auxiliar: crea archivo JSON válido con array raíz
// ---------------------------
procedure EnsureJsonFileExists(const FilePath, RootArrayName: string);
var
  jsonObject: TJSONObject;
  jsonArray: TJSONArray;
  jsonString: TStringList;
begin
  if not FileExists(FilePath) then
  begin
    jsonObject := TJSONObject.Create;
    jsonArray := TJSONArray.Create;
    jsonObject.Add(RootArrayName, jsonArray);

    jsonString := TStringList.Create;
    try
      jsonString.Text := jsonObject.FormatJSON();
      jsonString.SaveToFile(FilePath);
    finally
      jsonString.Free;
      jsonObject.Free;
    end;
  end;
end;

//
// ---------------------------
// Carga de usuarios desde JSON
// ---------------------------
function UploadUsersFromJson(const filePath: string): Boolean;
var
  jsonData: TJSONData = nil;
  jsonObject: TJSONObject;
  usersArray: TJSONArray;
  userItem: TJSONObject;
  i: Integer;
  content: TStringList;
begin
  Result := False;
  if not FileExists(filePath) then Exit;

  content := TStringList.Create;
  try
    content.LoadFromFile(filePath);
    jsonData := GetJSON(content.Text);

    jsonObject := TJSONObject(jsonData);
    usersArray := jsonObject.Arrays['usuarios'];

    for i := 0 to usersArray.Count - 1 do
    begin
      userItem := usersArray.Objects[i];
      LSL_U_Insert(
        IntToStr(userItem.Integers['id']),
        userItem.Strings['nombre'],
        userItem.Strings['email'],
        userItem.Strings['usuario'],
        userItem.Strings['telefono'],
        userItem.Strings['password']
      );
    end;
    Result := True;
  except
    on E: Exception do
      Writeln('Error leyendo JSON de usuarios: ', E.Message);
  end;

  content.Free;
  if Assigned(jsonData) then jsonData.Free;
end;

//
// ---------------------------
// Añadir un usuario al JSON
// ---------------------------
function AddUserToJson(const filePath: string; const id: Integer; const name, email, username, phone, password: string): Boolean;
var
  jsonData: TJSONData = nil;
  jsonObject: TJSONObject = nil;
  usersArray: TJSONArray = nil;
  newUser: TJSONObject;
  jsonString: TStringList;
  content: TStringList;
begin
  Result := False;
  EnsureJsonFileExists(filePath, 'usuarios');

  content := TStringList.Create;
  try
    content.LoadFromFile(filePath);
    jsonData := GetJSON(content.Text);

    jsonObject := TJSONObject(jsonData);
    usersArray := jsonObject.Arrays['usuarios'];

    newUser := TJSONObject.Create;
    newUser.Add('id', id);
    newUser.Add('nombre', name);
    newUser.Add('email', email);
    newUser.Add('usuario', username);
    newUser.Add('telefono', phone);
    newUser.Add('password', password);

    usersArray.Add(newUser);

    jsonString := TStringList.Create;
    try
      jsonString.Text := jsonObject.FormatJSON();
      jsonString.SaveToFile(filePath);
      Result := True;
    finally
      jsonString.Free;
    end;
  except
    on E: Exception do
      Writeln('Error añadiendo usuario al JSON: ', E.Message);
  end;

  content.Free;
  if Assigned(jsonData) then jsonData.Free;
end;

//
// ---------------------------
// Actualizar usuario existente
// ---------------------------
function UpdateUserInJson(const filePath: string; const email, username, phone: string): Boolean;
var
  jsonData: TJSONData = nil;
  jsonObject: TJSONObject;
  usersArray: TJSONArray;
  userItem: TJSONObject;
  i: Integer;
  jsonString: TStringList;
  content: TStringList;
begin
  Result := False;
  if not FileExists(filePath) then Exit;

  content := TStringList.Create;
  try
    content.LoadFromFile(filePath);
    jsonData := GetJSON(content.Text);

    jsonObject := TJSONObject(jsonData);
    usersArray := jsonObject.Arrays['usuarios'];

    for i := 0 to usersArray.Count - 1 do
    begin
      userItem := usersArray.Objects[i];
      if SameText(userItem.Strings['email'], email) then
      begin
        userItem.Strings['usuario'] := username;
        userItem.Strings['telefono'] := phone;
        Break;
      end;
    end;

    jsonString := TStringList.Create;
    try
      jsonString.Text := jsonObject.FormatJSON();
      jsonString.SaveToFile(filePath);
      Result := True;
    finally
      jsonString.Free;
    end;
  except
    on E: Exception do
      Writeln('Error actualizando JSON de usuario: ', E.Message);
  end;

  content.Free;
  if Assigned(jsonData) then jsonData.Free;
end;

//
// ---------------------------
// Añadir contacto al JSON
// ---------------------------
function AddContactToJson(const filePath, propietario, nombre, usuario, correo, telefono: string): Boolean;
var
  jsonData: TJSONData = nil;
  jsonObject: TJSONObject;
  contactsArray: TJSONArray;
  newContact: TJSONObject;
  jsonString: TStringList;
  content: TStringList;
begin
  Result := False;
  EnsureJsonFileExists(filePath, 'contactos');

  content := TStringList.Create;
  try
    content.LoadFromFile(filePath);
    jsonData := GetJSON(content.Text);

    jsonObject := TJSONObject(jsonData);
    contactsArray := jsonObject.Arrays['contactos'];

    newContact := TJSONObject.Create;
    newContact.Add('propietario', propietario);
    newContact.Add('nombre', nombre);
    newContact.Add('usuario', usuario);
    newContact.Add('correo', correo);
    newContact.Add('telefono', telefono);

    contactsArray.Add(newContact);

    jsonString := TStringList.Create;
    try
      jsonString.Text := jsonObject.FormatJSON();
      jsonString.SaveToFile(filePath);
      Result := True;
    finally
      jsonString.Free;
    end;
  except
    on E: Exception do
      Writeln('Error añadiendo contacto al JSON: ', E.Message);
  end;

  content.Free;
  if Assigned(jsonData) then jsonData.Free;
end;

//
// ---------------------------
// Cargar contactos desde JSON
// ---------------------------
function UploadContactsFromJson(const filePath, propietario: string; var contactList: PCNode): Boolean;
var
  jsonData: TJSONData = nil;
  jsonObject: TJSONObject;
  contactsArray: TJSONArray;
  contactItem: TJSONObject;
  i: Integer;
  content: TStringList;
begin
  Result := False;
  if not FileExists(filePath) then Exit;

  CL_ClearList(contactList);

  content := TStringList.Create;
  try
    content.LoadFromFile(filePath);
    jsonData := GetJSON(content.Text);
    jsonObject := TJSONObject(jsonData);
    contactsArray := jsonObject.Arrays['contactos'];

    for i := 0 to contactsArray.Count - 1 do
    begin
      contactItem := contactsArray.Objects[i];
      if SameText(contactItem.Strings['propietario'], propietario) then
      begin
        CL_InsertToList(
          contactList,
          contactItem.Strings['usuario'],   // id
          contactItem.Strings['nombre'],
          contactItem.Strings['correo'],
          contactItem.Strings['telefono']
        );
      end;
    end;
    Result := True;
  except
    on E: Exception do
      Writeln('Error leyendo contactos desde JSON: ', E.Message);
  end;

  content.Free;
  if Assigned(jsonData) then jsonData.Free;
end;

//
// ---------------------------
// Añadir correo al JSON
// ---------------------------
function AddMailToJson(const filePath, senderEmail, receiverEmail, subject, body: string): Boolean;
var
  jsonData: TJSONData = nil;
  jsonObject: TJSONObject;
  mailsArray: TJSONArray;
  newMail: TJSONObject;
  jsonString: TStringList;
  content: TStringList;
begin
  Result := False;

  // Asegurarse de que el archivo exista
  if not FileExists(filePath) then
  begin
    content := TStringList.Create;
    try
      content.Text := '{"mails":[]}';
      content.SaveToFile(filePath);
    finally
      content.Free;
    end;
  end;

  content := TStringList.Create;
  try
    content.LoadFromFile(filePath);
    jsonData := GetJSON(content.Text);
    jsonObject := TJSONObject(jsonData);
    mailsArray := jsonObject.Arrays['mails'];

    // Crear nuevo correo
    newMail := TJSONObject.Create;
    newMail.Add('sender', senderEmail);
    newMail.Add('receiver', receiverEmail);
    newMail.Add('subject', subject);
    newMail.Add('body', body);
    newMail.Add('timestamp', DateTimeToStr(Now));
    newMail.Add('estado', 'NL'); // No leído por defecto

    mailsArray.Add(newMail);

    // Guardar JSON
    jsonString := TStringList.Create;
    try
      jsonString.Text := jsonObject.FormatJSON();
      jsonString.SaveToFile(filePath);
      Result := True;
    finally
      jsonString.Free;
    end;

  except
    on E: Exception do
      Writeln('Error añadiendo correo al JSON: ', E.Message);
  end;

  content.Free;
  if Assigned(jsonData) then jsonData.Free;
end;

//
// ---------------------------
// Cargar correos recibidos (Inbox) desde JSON
// ---------------------------
procedure LoadInboxFromJson(const filePath, userEmail: string);
var
  jsonData: TJSONData = nil;
  jsonObject: TJSONObject;
  mailsArray: TJSONArray;
  mailObj: TJSONObject;
  i: Integer;
  estado, subject, sender, recipient, timestamp, body: string;
begin
  if not FileExists(filePath) then Exit;

  DL_ClearList; // limpiar lista antes de cargar

  try
    // Cargar contenido del archivo JSON
    with TStringList.Create do
    try
      LoadFromFile(filePath);
      if Text = '' then Exit;
      jsonData := GetJSON(Text);
    finally
      Free;
    end;

    // Obtener arreglo de correos
    jsonObject := TJSONObject(jsonData);
    mailsArray := jsonObject.Arrays['mails'];

    for i := 0 to mailsArray.Count - 1 do
    begin
      mailObj := mailsArray.Objects[i];

      recipient := mailObj.Get('receiver', '');
      if SameText(recipient, userEmail) then
      begin
        sender := mailObj.Get('sender', '');
        subject := mailObj.Get('subject', '');
        body := mailObj.Get('body', '');
        timestamp := mailObj.Get('timestamp', '');
        estado := mailObj.Get('estado', 'NL'); // si no existe, por defecto "No leído"

        // Insertar en lista doble
        DL_InsertToList(sender, subject, body, timestamp, estado);
      end;
    end;
  finally
    if Assigned(jsonData) then jsonData.Free;
  end;
end;

procedure LoadTrashFromJson(const filePath, userEmail: string);
var
  jsonData: TJSONData = nil;
  jsonObject: TJSONObject;
  mailsArray: TJSONArray;
  mailObj: TJSONObject;
  i: Integer;
  id, sender, recipient, subject, body, date: string;
begin
  if not FileExists(filePath) then Exit;

  // Limpiar pila actual antes de recargar
  Stack_Clear;

  try
    with TStringList.Create do
    try
      LoadFromFile(filePath);
      if Text = '' then Exit;
      jsonData := GetJSON(Text);
    finally
      Free;
    end;

    jsonObject := TJSONObject(jsonData);
    mailsArray := jsonObject.Arrays['mails'];

    for i := 0 to mailsArray.Count - 1 do
    begin
      mailObj := mailsArray.Objects[i];

      recipient := mailObj.Get('receiver', '');
      if SameText(recipient, userEmail) then
      begin
        id := IntToStr(i + 1);
        sender := mailObj.Get('sender', '');
        subject := mailObj.Get('subject', '');
        body := mailObj.Get('body', '');
        date := mailObj.Get('timestamp', '');

        // Insertar en la pila (tope = último leído)
        Stack_Push(id, sender, recipient, subject, body, date);
      end;
    end;

  finally
    if Assigned(jsonData) then jsonData.Free;
  end;
end;

//
// ---------------------------
// Mover un correo a la papelera
// ---------------------------
function MoveMailToTrash(const inboxPath, trashPath, userEmail, subject: string): Boolean;
var
  jsonData, trashData: TJSONData;
  jsonObject, mailObj, newMail, trashObj: TJSONObject;
  mailsArray, trashArray: TJSONArray;
  i: Integer;
  jsonString, trashString: TStringList;
  content, trashContent: TStringList;
begin
  Result := False;

  if not FileExists(inboxPath) then Exit;

  // Aseguramos que papelera.json exista
  EnsureJsonFileExists(trashPath, 'mails');

  content := TStringList.Create;
  try
    content.LoadFromFile(inboxPath);
    jsonData := GetJSON(content.Text);

    jsonObject := TJSONObject(jsonData);
    mailsArray := jsonObject.Arrays['mails'];

    for i := 0 to mailsArray.Count - 1 do
    begin
      mailObj := mailsArray.Objects[i];

      if SameText(mailObj.Strings['receiver'], userEmail) and
         SameText(mailObj.Strings['subject'], subject) then
      begin
        // 1) Copiar correo a trash.json
        newMail := TJSONObject.Create;
        newMail.Add('sender', mailObj.Strings['sender']);
        newMail.Add('receiver', mailObj.Strings['receiver']);
        newMail.Add('subject', mailObj.Strings['subject']);
        newMail.Add('body', mailObj.Strings['body']);
        newMail.Add('timestamp', mailObj.Strings['timestamp']);
        newMail.Add('estado', mailObj.Strings['estado']);

        trashContent := TStringList.Create;
        try
          trashContent.LoadFromFile(trashPath);
          trashData := GetJSON(trashContent.Text);
          trashObj := TJSONObject(trashData);
          trashArray := trashObj.Arrays['mails'];

          trashArray.Add(newMail);

          trashString := TStringList.Create;
          try
            trashString.Text := trashObj.FormatJSON();
            trashString.SaveToFile(trashPath);
          finally
            trashString.Free;
          end;
        finally
          trashContent.Free;
          if Assigned(trashData) then trashData.Free;
        end;

        // 2) Eliminar correo de inbox.json
        mailsArray.Delete(i);

        jsonString := TStringList.Create;
        try
          jsonString.Text := jsonObject.FormatJSON();
          jsonString.SaveToFile(inboxPath);
          Result := True;
        finally
          jsonString.Free;
        end;

        Break; // salir del bucle
      end;
    end;

  except
    on E: Exception do
      Writeln('Error moviendo correo a la papelera: ', E.Message);
  end;

  content.Free;
  if Assigned(jsonData) then jsonData.Free;
end;

//
// ---------------------------
// Guardar mensajes programados en JSON (desde la cola)
// ---------------------------
function SaveScheduledToJson(const filePath, userEmail: string): Boolean;
var
  jsonObject: TJSONObject;
  mailsArray: TJSONArray;
  jsonString: TStringList;
  Current: PMailQueueNode;
  newMail: TJSONObject;
begin
  Result := False;

  // Asegurar archivo
  EnsureJsonFileExists(filePath, 'scheduled');

  // Crear objeto raíz y array
  jsonObject := TJSONObject.Create;
  mailsArray := TJSONArray.Create;
  jsonObject.Add('scheduled', mailsArray);

  // Recorrer la cola y guardar
  Current := Queue_Peek;
  while Current <> nil do
  begin
    if SameText(Current^.sender, userEmail) then
    begin
      newMail := TJSONObject.Create;
      newMail.Add('id', Current^.id);
      newMail.Add('sender', Current^.sender);
      newMail.Add('receiver', Current^.recipient);
      newMail.Add('subject', Current^.subject);
      newMail.Add('body', Current^.message);
      newMail.Add('dateScheduled', Current^.dateScheduled);
      mailsArray.Add(newMail);
    end;
    Current := Current^.Next;
  end;

  // Guardar a archivo
  jsonString := TStringList.Create;
  try
    jsonString.Text := jsonObject.FormatJSON();
    jsonString.SaveToFile(filePath);
    Result := True;
  finally
    jsonString.Free;
    jsonObject.Free;
  end;
end;


//
// ---------------------------
// Cargar mensajes programados desde JSON a la cola
// ---------------------------
function LoadScheduledFromJson(const filePath, userEmail: string): Boolean;
var
  jsonData: TJSONData = nil;
  jsonObject: TJSONObject;
  mailsArray: TJSONArray;
  mailObj: TJSONObject;
  i: Integer;
begin
  Result := False;
  if not FileExists(filePath) then Exit;

  // Limpiar la cola antes de cargar
  Queue_Clear;

  try
    with TStringList.Create do
    try
      LoadFromFile(filePath);
      if Text = '' then Exit;
      jsonData := GetJSON(Text);
    finally
      Free;
    end;

    jsonObject := TJSONObject(jsonData);
    mailsArray := jsonObject.Arrays['scheduled'];

    for i := 0 to mailsArray.Count - 1 do
    begin
      mailObj := mailsArray.Objects[i];
      if SameText(mailObj.Get('sender', ''), userEmail) then
      begin
        Queue_Enqueue(
          mailObj.Get('id', ''),
          mailObj.Get('sender', ''),
          mailObj.Get('receiver', ''),
          mailObj.Get('subject', ''),
          mailObj.Get('body', ''),
          mailObj.Get('dateScheduled', '')
        );
      end;
    end;

    Result := True;
  finally
    if Assigned(jsonData) then jsonData.Free;
  end;
end;

//
// ---------------------------
// Enviar el primer correo programado de la cola al destinatario
// ---------------------------
function SendNextScheduledMail(const senderEmail: string): Boolean;
var
  nextMail: PMailQueueNode;
begin
  Result := False;

  // Revisar si hay correos en la cola
  if Queue_IsEmpty then Exit;

  nextMail := Queue_Peek; // obtener el primer correo

  // Solo enviar si el correo pertenece al usuario actual
  if not SameText(nextMail^.sender, senderEmail) then Exit;

  // Agregar correo al inbox del destinatario
  if AddMailToJson('inbox.json', nextMail^.sender, nextMail^.recipient, nextMail^.subject, nextMail^.message) then
  begin
    // Eliminar correo de la cola
    Queue_Dequeue;

    // Guardar cola actualizada en scheduled.json
    SaveScheduledToJson('scheduled.json', senderEmail);

    Result := True;
  end;
end;

function RemoveScheduledMailFromJson(const filePath, mailId: string): Boolean;
var
  jsonData: TJSONData = nil;
  jsonObject: TJSONObject;
  mailsArray: TJSONArray;
  i: Integer;
  jsonString: TStringList;
begin
  Result := False;
  if not FileExists(filePath) then Exit;

  try
    jsonString := TStringList.Create;
    try
      jsonString.LoadFromFile(filePath);
      jsonData := GetJSON(jsonString.Text);
      jsonObject := TJSONObject(jsonData);
      mailsArray := jsonObject.Arrays['scheduled'];

      // Buscar y eliminar correo por ID
      for i := mailsArray.Count - 1 downto 0 do
      begin
        if mailsArray.Objects[i].Get('id', '') = mailId then
        begin
          mailsArray.Delete(i);
          Break;
        end;
      end;

      // Guardar JSON actualizado
      jsonString.Text := jsonObject.FormatJSON();
      jsonString.SaveToFile(filePath);
      Result := True;
    finally
      jsonString.Free;
    end;
  finally
    if Assigned(jsonData) then jsonData.Free;
  end;
end;

end.