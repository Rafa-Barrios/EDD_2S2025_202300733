unit rootCom;

{$MODE DELPHI}

interface
  procedure ShowRootComWindow;

implementation

uses
  gtk2, glib2, SysUtils,
  fpjson, jsonparser, // ✅ NECESARIOS para TJSONObject, TJSONArray, etc.
  jsonTools, variables, interfaceTools,
  rootHome;

// --------------------------------------------------
// Variables globales de la ventana
// --------------------------------------------------
var
  comWindow: PGtkWidget;
  treeView: PGtkWidget;
  store: PGtkListStore;
  scroll: PGtkWidget;
  btnCancelar: PGtkWidget;

// --------------------------------------------------
// Cargar mensajes desde el JSON de comunidades
// --------------------------------------------------
procedure LoadCommunityMessages;
var
  jsonData: TJSONData;
  jsonObj: TJSONObject;
  comunidadesArray, mensajesArray: TJSONArray;
  comunidadObj, mensajeObj: TJSONObject;
  i, j: Integer;
  iter: TGtkTreeIter;
  comunidad, remitente, texto, fecha: string;
begin
  store := gtk_list_store_new(4, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);

  jsonData := LoadJSONDataFile(json_file_communities);
  if (jsonData = nil) then
  begin
    ShowErrorMessage(comWindow, 'Error', 'No se pudo abrir el archivo com.json.');
    Exit;
  end;

  try
    if jsonData.JSONType = jtObject then
    begin
      jsonObj := TJSONObject(jsonData);

      // Obtener el arreglo "comunidades"
      if jsonObj.IndexOfName('comunidades') <> -1 then
      begin
        comunidadesArray := jsonObj.Arrays['comunidades'];

        // Recorrer todas las comunidades
        for i := 0 to comunidadesArray.Count - 1 do
        begin
          comunidadObj := comunidadesArray.Objects[i];
          comunidad := comunidadObj.Get('Comunidad', '');
          fecha := comunidadObj.Get('fecha', '');

          // Lista de mensajes
          mensajesArray := comunidadObj.Arrays['Mensajes'];
          if mensajesArray <> nil then
          begin
            for j := 0 to mensajesArray.Count - 1 do
            begin
              mensajeObj := mensajesArray.Objects[j];
              remitente := mensajeObj.Get('remitente', '');
              texto := mensajeObj.Get('texto', '');

              gtk_list_store_append(store, @iter);
              gtk_list_store_set(store, @iter,
                                 0, PChar(comunidad),
                                 1, PChar(remitente),
                                 2, PChar(texto),
                                 3, PChar(fecha),
                                 -1);
            end;
          end;
        end;
      end
      else
        ShowErrorMessage(comWindow, 'Error', 'El JSON no contiene el campo "comunidades".');
    end
    else
      ShowErrorMessage(comWindow, 'Error', 'El formato del archivo JSON es inválido.');
  finally
    jsonData.Free;
  end;
end;

// --------------------------------------------------
// Botón Cancelar → Volver a RootHome
// --------------------------------------------------
procedure OnCancelClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_widget_destroy(comWindow);   // Cierra solo esta ventana
  ShowRootHomeWindow;              // Regresa al menú rootHome
end;

// --------------------------------------------------
// Mostrar ventana principal de comunidades
// --------------------------------------------------
procedure ShowRootComWindow;
var
  vbox, hbox, labelTitle: PGtkWidget;
  col: PGtkTreeViewColumn;
  renderer: PGtkCellRenderer;
begin
  comWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(comWindow), 'Mensajes de Comunidades');
  gtk_window_set_default_size(GTK_WINDOW(comWindow), 700, 400);
  gtk_container_set_border_width(GTK_CONTAINER(comWindow), 10);

  vbox := gtk_vbox_new(False, 10);
  gtk_container_add(GTK_CONTAINER(comWindow), vbox);

  labelTitle := gtk_label_new('Lista de Mensajes de Comunidades');
  gtk_box_pack_start(GTK_BOX(vbox), labelTitle, False, False, 0);

  scroll := gtk_scrolled_window_new(nil, nil);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll),
                                 GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_box_pack_start(GTK_BOX(vbox), scroll, True, True, 0);

  treeView := gtk_tree_view_new();
  gtk_container_add(GTK_CONTAINER(scroll), treeView);

  // Crear columnas
  renderer := gtk_cell_renderer_text_new();

  col := gtk_tree_view_column_new_with_attributes('Comunidad', renderer, 'text', 0, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), col);

  col := gtk_tree_view_column_new_with_attributes('Remitente', renderer, 'text', 1, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), col);

  col := gtk_tree_view_column_new_with_attributes('Mensaje', renderer, 'text', 2, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), col);

  col := gtk_tree_view_column_new_with_attributes('Fecha', renderer, 'text', 3, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), col);

  // Cargar datos
  LoadCommunityMessages();
  gtk_tree_view_set_model(GTK_TREE_VIEW(treeView), GTK_TREE_MODEL(store));

  // Botón Cancelar
  hbox := gtk_hbox_new(False, 10);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, False, False, 0);

  btnCancelar := gtk_button_new_with_label('Cancelar');
  g_signal_connect(btnCancelar, 'clicked', G_CALLBACK(@OnCancelClick), nil);
  gtk_box_pack_end(GTK_BOX(hbox), btnCancelar, False, False, 0);

  gtk_widget_show_all(comWindow);

  // Evita que cerrar esta ventana cierre todo el programa
  g_signal_connect(comWindow, 'destroy', G_CALLBACK(@gtk_widget_destroy), nil);
end;

end.

