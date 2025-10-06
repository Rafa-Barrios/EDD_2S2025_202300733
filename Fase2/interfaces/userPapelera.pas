unit userPapelera;

{$MODE DELPHI}

interface
procedure ShowUserPapeleraWindow;

implementation

uses
  gtk2, glib2, SysUtils, Classes, fpjson, jsonparser,
  variables, userHome, pila, jsonTools;

var
  papeleraWindow: PGtkWidget;
  btnClose, btnBuscar, btnEliminar: PGtkWidget;
  txtBuscar: PGtkWidget;
  treeView: PGtkWidget;
  listStore: PGtkListStore;

// ------------------------------
// Cerrar y volver al home
// ------------------------------
procedure OnCloseClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_widget_destroy(papeleraWindow);
  ShowUserHomeWindow;
end;

// ------------------------------
// Cargar datos de la pila en el TreeView (con filtro)
// ------------------------------
procedure LoadTrashToTreeView(const filter: string = '');
var
  iter: TGtkTreeIter;
  current: PMailStackNode;
  previewMsg: string;
  searchText: string;
begin
  gtk_list_store_clear(listStore); // limpiar tabla

  searchText := LowerCase(Trim(filter)); // 🔹 pasamos a minúsculas

  current := Stack_Peek;
  while current <> nil do
  begin
    // Verificar filtro (solo asunto)
    if (searchText = '') or
       (Pos(searchText, LowerCase(current^.subject)) > 0) then
    begin
      // recortar el mensaje para mostrar solo el inicio
      if Length(current^.message) > 50 then
        previewMsg := Copy(current^.message, 1, 50) + '...'
      else
        previewMsg := current^.message;

      gtk_list_store_append(listStore, @iter);
      gtk_list_store_set(listStore, @iter,
        0, PChar(current^.sender),
        1, PChar(current^.subject),
        2, PChar(previewMsg),
        -1);
    end;

    current := current^.Next;
  end;
end;

// ------------------------------
// Eliminar correo más reciente
// ------------------------------
procedure OnEliminarClick(widget: PGtkWidget; data: gpointer); cdecl;
var
  removedMail: PMailStackNode;
  i: Integer;
  jsonData: TJSONData = nil;
  jsonObject: TJSONObject;
  mailsArray: TJSONArray;
  content: TStringList;
begin
  // Sacar el correo más reciente de la pila
  removedMail := Stack_Pop;
  if removedMail = nil then Exit; // pila vacía

  // Actualizar trash.json
  content := TStringList.Create;
  try
    content.LoadFromFile(json_file_trash);
    jsonData := GetJSON(content.Text);
    jsonObject := TJSONObject(jsonData);
    mailsArray := jsonObject.Arrays['mails'];

    // Buscar y eliminar el correo que coincide con tope eliminado
    for i := mailsArray.Count - 1 downto 0 do
    begin
      if SameText(mailsArray.Objects[i].Get('receiver',''), removedMail^.recipient) and
         SameText(mailsArray.Objects[i].Get('subject',''), removedMail^.subject) then
      begin
        mailsArray.Delete(i);
        Break;
      end;
    end;

    content.Text := jsonObject.FormatJSON();
    content.SaveToFile(json_file_trash);

  finally
    content.Free;
    if Assigned(jsonData) then jsonData.Free;
  end;

  // Refrescar TreeView
  LoadTrashToTreeView;
end;

// ------------------------------
// Buscar correos por asunto
// ------------------------------
procedure OnBuscarClick(widget: PGtkWidget; data: gpointer); cdecl;
var
  text: PChar;
begin
  text := gtk_entry_get_text(GTK_ENTRY(txtBuscar));
  LoadTrashToTreeView(string(text));
end;

// ------------------------------
// Mostrar ventana de papelera
// ------------------------------
procedure ShowUserPapeleraWindow;
var
  vbox, hboxTop: PGtkWidget;
  scrolledWindow: PGtkWidget;
  col: PGtkTreeViewColumn;
  cell: PGtkCellRenderer;
begin
  gtk_init(@argc, @argv);

  // 🔹 Cargar la papelera desde trash.json a la pila
  LoadTrashFromJson(json_file_trash, current_user_email);

  // Ventana principal
  papeleraWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(papeleraWindow), 'Papelera');
  gtk_container_set_border_width(GTK_CONTAINER(papeleraWindow), 10);
  gtk_window_set_default_size(GTK_WINDOW(papeleraWindow), 600, 400);

  // Contenedor principal
  vbox := gtk_vbox_new(False, 5);
  gtk_container_add(GTK_CONTAINER(papeleraWindow), vbox);

  // Barra superior con entry + buscar + eliminar
  hboxTop := gtk_hbox_new(False, 5);
  gtk_box_pack_start(GTK_BOX(vbox), hboxTop, False, False, 0);

  // Campo de texto para búsqueda
  txtBuscar := gtk_entry_new;
  gtk_box_pack_start(GTK_BOX(hboxTop), txtBuscar, True, True, 0);

  // Botón Buscar
  btnBuscar := gtk_button_new_with_label('Buscar');
  gtk_box_pack_start(GTK_BOX(hboxTop), btnBuscar, False, False, 0);
  g_signal_connect(btnBuscar, 'clicked', G_CALLBACK(@OnBuscarClick), nil);

  // Botón Eliminar
  btnEliminar := gtk_button_new_with_label('Eliminar');
  gtk_box_pack_end(GTK_BOX(hboxTop), btnEliminar, False, False, 0);
  g_signal_connect(btnEliminar, 'clicked', G_CALLBACK(@OnEliminarClick), nil);

  // Tabla de correos
  scrolledWindow := gtk_scrolled_window_new(nil, nil);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledWindow),
                                 GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_box_pack_start(GTK_BOX(vbox), scrolledWindow, True, True, 0);

  treeView := gtk_tree_view_new;
  gtk_container_add(GTK_CONTAINER(scrolledWindow), treeView);

  listStore := gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);
  gtk_tree_view_set_model(GTK_TREE_VIEW(treeView), GTK_TREE_MODEL(listStore));

  // Columnas
  cell := gtk_cell_renderer_text_new;
  col := gtk_tree_view_column_new_with_attributes('Remitente', cell, 'text', 0, nil);
  gtk_tree_view_column_set_min_width(col, 200);
  gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), col);

  cell := gtk_cell_renderer_text_new;
  col := gtk_tree_view_column_new_with_attributes('Asunto', cell, 'text', 1, nil);
  gtk_tree_view_column_set_min_width(col, 250);
  gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), col);

  cell := gtk_cell_renderer_text_new;
  col := gtk_tree_view_column_new_with_attributes('Mensaje', cell, 'text', 2, nil);
  gtk_tree_view_column_set_min_width(col, 300);
  gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), col);

  // 🔹 Cargar datos desde la pila al TreeView
  LoadTrashToTreeView;

  // Botón cerrar (abajo)
  btnClose := gtk_button_new_with_label('Cerrar');
  g_signal_connect(btnClose, 'clicked', G_CALLBACK(@OnCloseClick), nil);
  gtk_box_pack_start(GTK_BOX(vbox), btnClose, False, False, 0);

  // Mostrar todo
  gtk_widget_show_all(papeleraWindow);

  g_signal_connect(papeleraWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

  gtk_main;
end;

end.
