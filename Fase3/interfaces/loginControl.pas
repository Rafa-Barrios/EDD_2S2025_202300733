unit loginControl;

{$MODE DELPHI}

interface

procedure ShowLoginControlWindow;

implementation

uses
  gtk2, glib2, SysUtils,
  variables, rootHome, jsonTools, fpjson, jsonparser;

var
  loginWindow: PGtkWidget;
  btnClose: PGtkWidget;
  treeView: PGtkWidget;
  listStore: PGtkListStore;

// -------------------------------------------------------------
// Cerrar ventana y volver al Home del root
// -------------------------------------------------------------
procedure OnCloseClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_widget_destroy(loginWindow);
  ShowRootHomeWindow; // ✅ vuelve a rootHome
end;

// -------------------------------------------------------------
// Cargar datos desde loginControl.json al TreeView
// -------------------------------------------------------------
procedure LoadLoginControlDataToTreeView;
var
  jsonData: TJSONData;
  jsonArray: TJSONArray;
  logObj: TJSONObject;
  i: Integer;
  iter: TGtkTreeIter;
begin
  gtk_list_store_clear(listStore);

  if not FileExists(json_file_login_control) then
    Exit;

  jsonData := LoadJSONDataFile(json_file_login_control);
  if not Assigned(jsonData) then
    Exit;

  if jsonData.JSONType <> jtArray then
  begin
    jsonData.Free;
    Exit;
  end;

  jsonArray := TJSONArray(jsonData);

  for i := 0 to jsonArray.Count - 1 do
  begin
    logObj := jsonArray.Objects[i];
    gtk_list_store_append(listStore, @iter);
    gtk_list_store_set(listStore, @iter,
      0, PChar(logObj.Strings['usuario']),
      1, PChar(logObj.Strings['entrada']),
      2, PChar(logObj.Strings['salida']),
      -1);
  end;

  jsonData.Free;
end;

// -------------------------------------------------------------
// Mostrar ventana principal de control de login
// -------------------------------------------------------------
procedure ShowLoginControlWindow;
var
  vbox, hboxTop: PGtkWidget;
  scrolledWindow: PGtkWidget;
  col: PGtkTreeViewColumn;
  cell: PGtkCellRenderer;
  lblTitle: PGtkWidget;
begin
  gtk_init(@argc, @argv);

  // Ventana principal
  loginWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(loginWindow), 'Control de Inicios y Cierres de Sesión');
  gtk_container_set_border_width(GTK_CONTAINER(loginWindow), 10);
  gtk_window_set_default_size(GTK_WINDOW(loginWindow), 700, 400);

  // Contenedor principal
  vbox := gtk_vbox_new(False, 5);
  gtk_container_add(GTK_CONTAINER(loginWindow), vbox);

  // Barra superior
  hboxTop := gtk_hbox_new(False, 5);
  lblTitle := gtk_label_new('Historial de Sesiones');
  gtk_box_pack_start(GTK_BOX(hboxTop), lblTitle, False, False, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hboxTop, False, False, 0);

  // Tabla con scroll
  scrolledWindow := gtk_scrolled_window_new(nil, nil);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledWindow),
                                 GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_box_pack_start(GTK_BOX(vbox), scrolledWindow, True, True, 0);

  treeView := gtk_tree_view_new;
  gtk_container_add(GTK_CONTAINER(scrolledWindow), treeView);

  // Crear modelo
  listStore := gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);
  gtk_tree_view_set_model(GTK_TREE_VIEW(treeView), GTK_TREE_MODEL(listStore));

  // Columnas
  cell := gtk_cell_renderer_text_new;
  col := gtk_tree_view_column_new_with_attributes('Usuario', cell, 'text', 0, nil);
  gtk_tree_view_column_set_min_width(col, 150);
  gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), col);

  cell := gtk_cell_renderer_text_new;
  col := gtk_tree_view_column_new_with_attributes('Entrada', cell, 'text', 1, nil);
  gtk_tree_view_column_set_min_width(col, 250);
  gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), col);

  cell := gtk_cell_renderer_text_new;
  col := gtk_tree_view_column_new_with_attributes('Salida', cell, 'text', 2, nil);
  gtk_tree_view_column_set_min_width(col, 250);
  gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), col);

  // Botón cerrar
  btnClose := gtk_button_new_with_label('Cerrar');
  g_signal_connect(btnClose, 'clicked', G_CALLBACK(@OnCloseClick), nil);
  gtk_box_pack_start(GTK_BOX(vbox), btnClose, False, False, 0);

  // Cargar datos del JSON
  LoadLoginControlDataToTreeView;

  // Mostrar todo
  gtk_widget_show_all(loginWindow);

  g_signal_connect(loginWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

  gtk_main;
end;

end.
