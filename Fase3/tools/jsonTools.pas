unit jsonTools;

{$mode objfpc}{$H+}

interface

uses
  variables,
  simpleLinkedList,
  doubleLinkedList,
  circularLinkedList,
  Classes, SysUtils, fpjson, jsonparser, pila, cola;

function UploadUsersFromJson(const filePath: string): Boolean;
function AddUserToJson(const filePath: string; const id: Integer; const name, email, username, phone, password: string): Boolean;
function UpdateUserInJson(const filePath: string; const email, username, phone: string): Boolean;

function AddContactToJson(const filePath, propietario, nombre, usuario, correo, telefono: string): Boolean;
function UploadContactsFromJson(const filePath, propietario: string; var contactList: PCNode): Boolean;
function RemoveContactFromJson(const filePath, propietario, email: string): Boolean;

function AddMailToJson(const filePath, senderEmail, receiverEmail, subject, body: string; mailID: Integer): Boolean;
procedure LoadInboxFromJson(const filePath, userEmail: string);
procedure LoadTrashFromJson(const filePath, userEmail: string);
function MoveMailToTrash(const inboxPath, trashPath, userEmail, subject: string): Boolean;

function SaveScheduledToJson(const filePath, userEmail: string): Boolean;
function LoadScheduledFromJson(const filePath, userEmail: string): Boolean;
function SendNextScheduledMail(const senderEmail: string): Boolean;
function RemoveScheduledMailFromJson(const filePath, mailId: string): Boolean;

function GetAvailableId(const filePath: String; MaxID: Integer): Integer;
function ReadFileToString(const FileName: string): AnsiString;

function AddDraftToJson(const filePath, senderEmail, receiverEmail, subject, body: string; mailID: Integer): Boolean;
procedure LoadDraftsFromJson(const filePath, userEmail: string);
function RemoveDraftFromJson(const filePath: string; mailID: Integer): Boolean;

procedure SaveCommunityMessage(const communityName, messageText, senderEmail: string);
function LoadJSONDataFile(const FileName: string): TJSONData;

procedure RegisterLoginEntry(const username: string);
procedure RegisterLogout(const username: string);

implementation

// -----------------------------------------------------------------------------
// --------------------------- UTILIDADES JSON -------------------------------
// -----------------------------------------------------------------------------

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

function ReadFileToString(const FileName: string): AnsiString;
var
  F: TextFile;
  Line: string;
begin
  Result := '';
  if not FileExists(FileName) then Exit;
  AssignFile(F, FileName);
  Reset(F);
  try
    while not EOF(F) do
    begin
      ReadLn(F, Line);
      Result := Result + Line + LineEnding;
    end;
  finally
    CloseFile(F);
  end;
end;

// Implementación de la nueva función de carga
function LoadJSONDataFile(const FileName: string): TJSONData;
var
  content: AnsiString;
begin
  Result := nil;
  if not FileExists(FileName) then Exit;

  content := ReadFileToString(FileName);
  if Trim(content) = '' then Exit;
  
  // Asumimos que GetJSON es una función disponible (ya lo es en esta unidad)
  // que convierte la cadena de texto a TJSONData.
  Result := GetJSON(content);
end;

// -----------------------------------------------------------------------------
// --------------------------- USUARIOS ---------------------------------------
// -----------------------------------------------------------------------------

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
  finally
    content.Free;
    if Assigned(jsonData) then jsonData.Free;
  end;
end;

function AddUserToJson(const filePath: string; const id: Integer; const name, email, username, phone, password: string): Boolean;
var
  jsonData: TJSONData = nil;
  jsonObject: TJSONObject;
  usersArray: TJSONArray;
  newUser: TJSONObject;
  jsonString, content: TStringList;
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
  finally
    content.Free;
    if Assigned(jsonData) then jsonData.Free;
  end;
end;

function UpdateUserInJson(const filePath: string; const email, username, phone: string): Boolean;
var
  jsonData: TJSONData = nil;
  jsonObject: TJSONObject;
  usersArray: TJSONArray;
  userItem: TJSONObject;
  i: Integer;
  jsonString, content: TStringList;
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
  finally
    content.Free;
    if Assigned(jsonData) then jsonData.Free;
  end;
end;

// -----------------------------------------------------------------------------
// --------------------------- CONTACTOS --------------------------------------
// -----------------------------------------------------------------------------

function AddContactToJson(const filePath, propietario, nombre, usuario, correo, telefono: string): Boolean;
var
  jsonData: TJSONData = nil;
  jsonObject: TJSONObject;
  contactsArray: TJSONArray;
  newContact: TJSONObject;
  jsonString, content: TStringList;
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
  finally
    content.Free;
    if Assigned(jsonData) then jsonData.Free;
  end;
end;

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
          contactItem.Strings['usuario'],
          contactItem.Strings['nombre'],
          contactItem.Strings['correo'],
          contactItem.Strings['telefono']
        );
      end;
    end;
    Result := True;
  finally
    content.Free;
    if Assigned(jsonData) then jsonData.Free;
  end;
end;

function RemoveContactFromJson(const filePath, propietario, email: string): Boolean;
var
  jsonData: TJSONData = nil;
  jsonObject: TJSONObject;
  contactsArray: TJSONArray;
  i: Integer;
  jsonString: TStringList;
begin
  Result := False;
  if not FileExists(filePath) then Exit;

  jsonString := TStringList.Create;
  try
    jsonString.LoadFromFile(filePath);
    jsonData := GetJSON(jsonString.Text);
    jsonObject := TJSONObject(jsonData);
    contactsArray := jsonObject.Arrays['contactos'];

    for i := contactsArray.Count - 1 downto 0 do
    begin
      if SameText(contactsArray.Objects[i].Strings['propietario'], propietario) and
         SameText(contactsArray.Objects[i].Strings['correo'], email) then
      begin
        contactsArray.Delete(i);
        Result := True;
        Break;
      end;
    end;

    jsonString.Text := jsonObject.FormatJSON();
    jsonString.SaveToFile(filePath);
  finally
    jsonString.Free;
    if Assigned(jsonData) then jsonData.Free;
  end;
end;

// -----------------------------------------------------------------------------
// --------------------------- CORREOS ----------------------------------------
// -----------------------------------------------------------------------------

function AddMailToJson(const filePath, senderEmail, receiverEmail, subject, body: string; mailID: Integer): Boolean;
var
  jsonData: TJSONData = nil;
  jsonObject: TJSONObject;
  mailsArray: TJSONArray;
  newMail: TJSONObject;
  jsonString, content: TStringList;
begin
  Result := False;

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

    newMail := TJSONObject.Create;
    newMail.Add('id', mailID);
    newMail.Add('remitente', senderEmail);
    newMail.Add('destinatario', receiverEmail);
    newMail.Add('asunto', subject);
    newMail.Add('mensaje', body);
    newMail.Add('estado', 'NL');

    mailsArray.Add(newMail);

    jsonString := TStringList.Create;
    try
      jsonString.Text := jsonObject.FormatJSON();
      jsonString.SaveToFile(filePath);
      Result := True;
    finally
      jsonString.Free;
    end;
  finally
    content.Free;
    if Assigned(jsonData) then jsonData.Free;
  end;
end;

procedure LoadInboxFromJson(const filePath, userEmail: string);
var
  jsonData: TJSONData = nil;
  jsonObject: TJSONObject;
  mailsArray: TJSONArray;
  mailObj: TJSONObject;
  i: Integer;
  idInt: QWord;
  remitente, destinatario, asunto, mensaje, estado: string;
begin
  if not FileExists(filePath) then Exit;

  DL_ClearList;

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

      destinatario := mailObj.Get('destinatario', '');
      if SameText(destinatario, userEmail) then
      begin
        idInt := mailObj.Integers['id'];
        remitente := mailObj.Get('remitente', '');
        asunto := mailObj.Get('asunto', '');
        mensaje := mailObj.Get('mensaje', '');
        estado := mailObj.Get('estado', 'NL');

        DL_InsertToList(IntToStr(idInt), remitente, asunto, mensaje, DateTimeToStr(Now), estado);
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
  idInt: QWord;
  remitente, destinatario, asunto, mensaje: string;
begin
  if not FileExists(filePath) then Exit;

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

      destinatario := mailObj.Get('destinatario', '');
      if SameText(destinatario, userEmail) then
      begin
        idInt := mailObj.Integers['id'];
        remitente := mailObj.Get('remitente', '');
        asunto := mailObj.Get('asunto', '');
        mensaje := mailObj.Get('mensaje', '');

        Stack_Push(IntToStr(idInt), remitente, destinatario, asunto, mensaje, DateTimeToStr(Now));
      end;
    end;
  finally
    if Assigned(jsonData) then jsonData.Free;
  end;
end;

function MoveMailToTrash(const inboxPath, trashPath, userEmail, subject: string): Boolean;
var
  jsonData, trashData: TJSONData;
  jsonObject, trashObject, mailObj, newMail: TJSONObject;
  mailsArray, trashArray: TJSONArray;
  i: Integer;
  content, trashContent, jsonString, trashString: TStringList;
  destinatario, asunto: string;
  idInt: QWord;
begin
  Result := False;
  if not FileExists(inboxPath) then Exit;

  EnsureJsonFileExists(trashPath, 'mails');

  content := TStringList.Create;
  try
    content.LoadFromFile(inboxPath);
    jsonData := GetJSON(content.Text);
    jsonObject := TJSONObject(jsonData);
    mailsArray := jsonObject.Arrays['mails'];

    for i := mailsArray.Count - 1 downto 0 do
    begin
      mailObj := mailsArray.Objects[i];
      destinatario := mailObj.Get('destinatario', '');
      asunto := mailObj.Get('asunto', '');
      if SameText(destinatario, userEmail) and SameText(asunto, subject) then
      begin
        idInt := mailObj.Integers['id'];

        // Copiar correo a trash.json
        newMail := TJSONObject.Create;
        newMail.Add('id', idInt);
        newMail.Add('remitente', mailObj.Get('remitente', ''));
        newMail.Add('destinatario', mailObj.Get('destinatario', ''));
        newMail.Add('asunto', mailObj.Get('asunto', ''));
        newMail.Add('mensaje', mailObj.Get('mensaje', ''));
        newMail.Add('estado', mailObj.Get('estado', 'NL'));

        trashContent := TStringList.Create;
        try
          trashContent.LoadFromFile(trashPath);
          trashData := GetJSON(trashContent.Text);
          trashObject := TJSONObject(trashData);
          trashArray := trashObject.Arrays['mails'];
          trashArray.Add(newMail);

          trashString := TStringList.Create;
          try
            trashString.Text := trashObject.FormatJSON();
            trashString.SaveToFile(trashPath);
          finally
            trashString.Free;
          end;
        finally
          trashContent.Free;
          if Assigned(trashData) then trashData.Free;
        end;

        // Eliminar correo de inbox.json
        mailsArray.Delete(i);

        jsonString := TStringList.Create;
        try
          jsonString.Text := jsonObject.FormatJSON();
          jsonString.SaveToFile(inboxPath);
          Result := True;
        finally
          jsonString.Free;
        end;

      end;
    end;
  finally
    content.Free;
    if Assigned(jsonData) then jsonData.Free;
  end;
end;

// -----------------------------------------------------------------------------
// ------------------- CORREOS PROGRAMADOS (COLA) -----------------------------
// -----------------------------------------------------------------------------

function SaveScheduledToJson(const filePath, userEmail: string): Boolean;
var
  jsonObject: TJSONObject;
  mailsArray: TJSONArray;
  jsonString: TStringList;
  Current: PMailQueueNode;
  newMail: TJSONObject;
begin
  Result := False;
  EnsureJsonFileExists(filePath, 'scheduled');

  jsonObject := TJSONObject.Create;
  mailsArray := TJSONArray.Create;
  jsonObject.Add('scheduled', mailsArray);

  Current := Queue_Peek;
  while Current <> nil do
  begin
    if SameText(Current^.sender, userEmail) then
    begin
      newMail := TJSONObject.Create;
      newMail.Add('id', StrToIntDef(Current^.id, 0));
      newMail.Add('remitente', Current^.sender);
      newMail.Add('destinatario', Current^.recipient);
      newMail.Add('asunto', Current^.subject);
      newMail.Add('mensaje', Current^.message);
      newMail.Add('dateScheduled', Current^.dateScheduled);
      mailsArray.Add(newMail);
    end;
    Current := Current^.Next;
  end;

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
      if SameText(mailObj.Get('remitente', ''), userEmail) then
      begin
        Queue_Enqueue(
          IntToStr(mailObj.Integers['id']),
          mailObj.Get('remitente', ''),
          mailObj.Get('destinatario', ''),
          mailObj.Get('asunto', ''),
          mailObj.Get('mensaje', ''),
          mailObj.Get('dateScheduled', '')
        );
      end;
    end;

    Result := True;
  finally
    if Assigned(jsonData) then jsonData.Free;
  end;
end;

function SendNextScheduledMail(const senderEmail: string): Boolean;
var
  nextMail: PMailQueueNode;
begin
  Result := False;
  if Queue_IsEmpty then Exit;

  nextMail := Queue_Peek;
  if not SameText(nextMail^.sender, senderEmail) then Exit;

  if AddMailToJson('inbox.json', nextMail^.sender, nextMail^.recipient, nextMail^.subject, nextMail^.message, StrToIntDef(nextMail^.id, 0)) then
  begin
    Queue_Dequeue;
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

  jsonString := TStringList.Create;
  try
    jsonString.LoadFromFile(filePath);
    jsonData := GetJSON(jsonString.Text);
    jsonObject := TJSONObject(jsonData);
    mailsArray := jsonObject.Arrays['scheduled'];

    for i := mailsArray.Count - 1 downto 0 do
    begin
      if IntToStr(mailsArray.Objects[i].Integers['id']) = mailId then
      begin
        mailsArray.Delete(i);
        Result := True;
        Break;
      end;
    end;

    jsonString.Text := jsonObject.FormatJSON();
    jsonString.SaveToFile(filePath);
  finally
    jsonString.Free;
    if Assigned(jsonData) then jsonData.Free;
  end;
end;

function AddDraftToJson(const filePath, senderEmail, receiverEmail, subject, body: string; mailID: Integer): Boolean;
var
  jsonData: TJSONData = nil;
  jsonObject: TJSONObject;
  draftsArray: TJSONArray;
  newDraft: TJSONObject;
  jsonString, content: TStringList;
begin
  Result := False;
  EnsureJsonFileExists(filePath, 'mails');

  content := TStringList.Create;
  try
    content.LoadFromFile(filePath);
    jsonData := GetJSON(content.Text);
    jsonObject := TJSONObject(jsonData);
    draftsArray := jsonObject.Arrays['mails'];

    newDraft := TJSONObject.Create;
    newDraft.Add('id', mailID);
    newDraft.Add('remitente', senderEmail);
    newDraft.Add('destinatario', receiverEmail);
    newDraft.Add('asunto', subject);
    newDraft.Add('mensaje', body);

    draftsArray.Add(newDraft);

    jsonString := TStringList.Create;
    try
      jsonString.Text := jsonObject.FormatJSON();
      jsonString.SaveToFile(filePath);
      Result := True;
    finally
      jsonString.Free;
    end;
  finally
    content.Free;
    if Assigned(jsonData) then jsonData.Free;
  end;
end;

procedure LoadDraftsFromJson(const filePath, userEmail: string);
var
  jsonData: TJSONData = nil;
  jsonObject: TJSONObject;
  draftsArray: TJSONArray;
  draftObj: TJSONObject;
  i: Integer;
  idInt: QWord;
  remitente, destinatario, asunto, mensaje: string;
begin
  if not FileExists(filePath) then Exit;

  DL_ClearList; // opcional si quieres usar la misma lista doble

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
    draftsArray := jsonObject.Arrays['mails'];

    for i := 0 to draftsArray.Count - 1 do
    begin
      draftObj := draftsArray.Objects[i];
      remitente := draftObj.Get('remitente', '');
      if SameText(remitente, userEmail) then
      begin
        idInt := draftObj.Integers['id'];
        destinatario := draftObj.Get('destinatario', '');
        asunto := draftObj.Get('asunto', '');
        mensaje := draftObj.Get('mensaje', '');

        DL_InsertToList(IntToStr(idInt), remitente, asunto, mensaje, DateTimeToStr(Now), 'DR'); // DR = draft
      end;
    end;
  finally
    if Assigned(jsonData) then jsonData.Free;
  end;
end;

function RemoveDraftFromJson(const filePath: string; mailID: Integer): Boolean;
var
  jsonData: TJSONData = nil;
  jsonObject: TJSONObject;
  draftsArray: TJSONArray;
  i: Integer;
  jsonString: TStringList;
begin
  Result := False;
  if not FileExists(filePath) then Exit;

  jsonString := TStringList.Create;
  try
    jsonString.LoadFromFile(filePath);
    jsonData := GetJSON(jsonString.Text);
    jsonObject := TJSONObject(jsonData);
    draftsArray := jsonObject.Arrays['mails'];

    for i := draftsArray.Count - 1 downto 0 do
    begin
      if draftsArray.Objects[i].Integers['id'] = mailID then
      begin
        draftsArray.Delete(i);
        Result := True;
        Break;
      end;
    end;

    jsonString.Text := jsonObject.FormatJSON();
    jsonString.SaveToFile(filePath);
  finally
    jsonString.Free;
    if Assigned(jsonData) then jsonData.Free;
  end;
end;

// -------------------------------------------------------------
// Guarda un mensaje de comunidad (crear o actualizar com.json)
// -------------------------------------------------------------
procedure SaveCommunityMessage(const communityName, messageText, senderEmail: string);
var
  jsonFilePath: string;
  jsonData, newCommunityObj, existingCommunity, newMessage: TJSONObject;
  communityArray, messagesArray: TJSONArray;
  jsonParser: TJSONParser;
  fileContent: TStringList;
  i: Integer;
  found: Boolean;
  fechaActual: string;
begin
  jsonFilePath := json_file_communities;
  fechaActual := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  // Si el archivo existe, lo leemos
  if FileExists(jsonFilePath) then
  begin
    fileContent := TStringList.Create;
    try
      fileContent.LoadFromFile(jsonFilePath);
      jsonParser := TJSONParser.Create(fileContent.Text);
      jsonData := jsonParser.Parse as TJSONObject;
    finally
      fileContent.Free;
      jsonParser.Free;
    end;
  end
  else
  begin
    // Crear nuevo JSON base si no existe
    jsonData := TJSONObject.Create;
    jsonData.Add('comunidades', TJSONArray.Create);
  end;

  // Obtenemos arreglo de comunidades
  if jsonData.Find('comunidades') <> nil then
    communityArray := jsonData.Arrays['comunidades']
  else
  begin
    communityArray := TJSONArray.Create;
    jsonData.Add('comunidades', communityArray);
  end;

  // Buscar si la comunidad ya existe
  found := False;
  for i := 0 to communityArray.Count - 1 do
  begin
    existingCommunity := communityArray.Objects[i];
    if SameText(existingCommunity.Strings['Comunidad'], communityName) then
    begin
      found := True;
      // Añadimos nuevo mensaje
      messagesArray := existingCommunity.Arrays['Mensajes'];

      newMessage := TJSONObject.Create;
      newMessage.Add('remitente', senderEmail);
      newMessage.Add('texto', messageText);
      messagesArray.Add(newMessage);

      // Actualizamos contador y fecha
      existingCommunity.Integers['mensajes'] := messagesArray.Count;
      existingCommunity.Strings['fecha'] := fechaActual;
      Break;
    end;
  end;

  // Si la comunidad no existía, la creamos
  if not found then
  begin
    newCommunityObj := TJSONObject.Create;
    messagesArray := TJSONArray.Create;

    newMessage := TJSONObject.Create;
    newMessage.Add('remitente', senderEmail);
    newMessage.Add('texto', messageText);
    messagesArray.Add(newMessage);

    newCommunityObj.Add('Comunidad', communityName);
    newCommunityObj.Add('mensajes', 1);
    newCommunityObj.Add('fecha', fechaActual);
    newCommunityObj.Add('Mensajes', messagesArray);

    communityArray.Add(newCommunityObj);
  end;

  // Guardar todo de nuevo en el archivo
  fileContent := TStringList.Create;
  try
    fileContent.Text := jsonData.FormatJSON([]);
    fileContent.SaveToFile(jsonFilePath);
  finally
    fileContent.Free;
    jsonData.Free;
  end;
end;

// -----------------------------------------------------------------------------
// --------------------------- OTROS -----------------------------------------
// -----------------------------------------------------------------------------

function GetAvailableId(const filePath: String; MaxID: Integer): Integer;
var
  used: array of Integer;
  candidate, i: Integer;
  jsonData: TJSONData;
  jsonObject: TJSONObject;
  mailArray: TJSONArray;
  mailObject: TJSONObject;
  j, count: Integer;
  idUsed: Boolean;
begin
  SetLength(used, 0);
  if FileExists(filePath) then
  try
    jsonData := GetJSON(ReadFileToString(filePath));
    if (jsonData.JSONType = jtObject) then
    begin
      jsonObject := TJSONObject(jsonData);
      if jsonObject.IndexOfName('mails') <> -1 then
      begin
        mailArray := jsonObject.Arrays['mails'];
        count := mailArray.Count;
        for j := 0 to count - 1 do
        begin
          mailObject := mailArray.Objects[j];
          if mailObject.IndexOfName('id') <> -1 then
          begin
            SetLength(used, Length(used)+1);
            used[High(used)] := mailObject.Integers['id'];
          end;
        end;
      end;
    end;
  except
    on E: Exception do Writeln('Error GetAvailableId: ', E.Message);
  end;

  repeat
    candidate := Random(MaxID)+1;
    idUsed := False;
    for i := 0 to High(used) do
      if used[i] = candidate then
      begin
        idUsed := True;
        Break;
      end;
  until not idUsed;

  Result := candidate;
end;

// -----------------------------------------------------------------------------
// --------------------------- CONTROL DE LOGUEO ------------------------------
// -----------------------------------------------------------------------------

procedure RegisterLoginEntry(const username: string);
  var
    jsonArray: TJSONArray;
    jsonItem: TJSONObject;
    jsonString: TStringList;
    nowStr: string;
    jsonData: TJSONData;
  begin
    nowStr := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

    // Si el archivo no existe, crearlo vacío con []
    if not FileExists(json_file_login_control) then
    begin
      jsonString := TStringList.Create;
      try
        jsonString.Text := '[]';
        jsonString.SaveToFile(json_file_login_control);
      finally
        jsonString.Free;
      end;
    end;

    jsonData := GetJSON(ReadFileToString(json_file_login_control));
    jsonArray := TJSONArray(jsonData);

    // Crear nueva entrada
    jsonItem := TJSONObject.Create;
    jsonItem.Add('usuario', username);
    jsonItem.Add('entrada', nowStr);
    jsonItem.Add('salida', '');

    jsonArray.Add(jsonItem);

    // Guardar cambios
    jsonString := TStringList.Create;
    try
      jsonString.Text := jsonArray.FormatJSON([]);
      jsonString.SaveToFile(json_file_login_control);
    finally
      jsonString.Free;
      jsonArray.Free;
    end;
  end;

procedure RegisterLogout(const username: string);
  var
    jsonData: TJSONData;
    jsonArray: TJSONArray;
    item: TJSONObject;
    jsonString: TStringList;
    i: Integer;
    nowStr: string;
  begin
    if not FileExists(json_file_login_control) then Exit;

    jsonData := GetJSON(ReadFileToString(json_file_login_control));
    jsonArray := TJSONArray(jsonData);

    nowStr := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

    // Buscar último registro del usuario sin salida
    for i := jsonArray.Count - 1 downto 0 do
    begin
      item := jsonArray.Objects[i];
      if SameText(item.Strings['usuario'], username) and (Trim(item.Strings['salida']) = '') then
      begin
        item.Strings['salida'] := nowStr;
        Break;
      end;
    end;

    jsonString := TStringList.Create;
    try
      jsonString.Text := jsonArray.FormatJSON([]);
      jsonString.SaveToFile(json_file_login_control);
    finally
      jsonString.Free;
      jsonArray.Free;
    end;
end;

end.