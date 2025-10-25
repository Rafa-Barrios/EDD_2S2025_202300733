unit loginControl;

{$MODE DELPHI}

interface
procedure ShowLoginControlWindow;

implementation

uses
  gtk2, glib2, SysUtils,
  variables, rootHome, jsonTools,
  fpjson, jsonparser;  // 👈 para manejar JSON

var
  loginWindow: PGtkWidget;
  btnClose: PGtkWidget;
  treeView: PGtkWidget;
  listStore: PGtkListStore;

// -------------------------------------------------------------------
// Regresar al root home
// -------------------------------------------------------------------
procedure OnCloseClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_widget_destroy(loginWindow);
  ShowRootHomeWindow;
end;

// -------------------------------------------------------------------
// Cargar el contenido de login.json al TreeView
// -------------------------------------------------------------------
procedure LoadLoginsToTreeView;
var
  jsonData: TJSONData;
  jsonArray: TJSONArray;
  jsonObj: TJSONObject;
  iter: TGtkTreeIter;
  i: Integer;
  usuario, entrada, salida: String;
  filePath: String;
  jsonText: TStringList;
begin
  filePath := 'data/login.json';
  gtk_list_store_clear(listStore);

  // Si el archivo no existe, no hay datos que mostrar
  if not FileExists(filePath) then Exit;

  // Cargar contenido del archivo
  jsonText := TStringList.Create;
  try
    jsonText.LoadFromFile(filePath);
    jsonData := GetJSON(jsonText.Text);

    if jsonData.JSONType <> jtArray then
      Exit;

    jsonArray := TJSONArray(jsonData);

    for i := 0 to jsonArray.Count - 1 do
    begin
      jsonObj := jsonArray.Objects[i];
      usuario := jsonObj.Get('usuario', '');
      entrada := jsonObj.Get('entrada', '');
      salida  := jsonObj.Get('salida', '');

      gtk_list_store_append(listStore, @iter);
      gtk_list_store_set(listStore, @iter,
        0, PChar(usuario),
        1, PChar(entrada),
        2, PChar(salida),
        -1);
    end;
  finally
    jsonText.Free;
    jsonData.Free;
  end;
end;

// -------------------------------------------------------------------
// Mostrar la ventana principal del control de logins
// -------------------------------------------------------------------
procedure ShowLoginControlWindow;
var
  vbox: PGtkWidget;
  scrolledWindow: PGtkWidget;
  col: PGtkTreeViewColumn;
  cell: PGtkCellRenderer;
begin
  gtk_init(@argc, @argv);

  // Ventana principal
  loginWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(loginWindow), 'Control de Logins');
  gtk_container_set_border_width(GTK_CONTAINER(loginWindow), 10);
  gtk_window_set_default_size(GTK_WINDOW(loginWindow), 600, 400);

  // Contenedor vertical
  vbox := gtk_vbox_new(False, 5);
  gtk_container_add(GTK_CONTAINER(loginWindow), vbox);

  // Tabla con scroll
  scrolledWindow := gtk_scrolled_window_new(nil, nil);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledWindow),
                                 GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_box_pack_start(GTK_BOX(vbox), scrolledWindow, True, True, 0);

  treeView := gtk_tree_view_new;
  gtk_container_add(GTK_CONTAINER(scrolledWindow), treeView);

  // Modelo de datos: usuario, entrada, salida
  listStore := gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);
  gtk_tree_view_set_model(GTK_TREE_VIEW(treeView), GTK_TREE_MODEL(listStore));

  // Columnas
  cell := gtk_cell_renderer_text_new;
  col := gtk_tree_view_column_new_with_attributes('Usuario', cell, 'text', 0, nil);
  gtk_tree_view_column_set_min_width(col, 200);
  gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), col);

  cell := gtk_cell_renderer_text_new;
  col := gtk_tree_view_column_new_with_attributes('Entrada', cell, 'text', 1, nil);
  gtk_tree_view_column_set_min_width(col, 200);
  gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), col);

  cell := gtk_cell_renderer_text_new;
  col := gtk_tree_view_column_new_with_attributes('Salida', cell, 'text', 2, nil);
  gtk_tree_view_column_set_min_width(col, 200);
  gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), col);

  // Botón cerrar
  btnClose := gtk_button_new_with_label('Regresar');
  g_signal_connect(btnClose, 'clicked', G_CALLBACK(@OnCloseClick), nil);
  gtk_box_pack_start(GTK_BOX(vbox), btnClose, False, False, 0);

  // Cargar datos desde el JSON
  LoadLoginsToTreeView;

  // Mostrar todo
  gtk_widget_show_all(loginWindow);
  g_signal_connect(loginWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

  gtk_main;
end;

end.

