unit userDraft;

{$MODE DELPHI}

interface
procedure ShowUserDraftsWindow;
procedure ReloadDrafts; // Para refrescar la vista de borradores

implementation

uses
  gtk2, glib2, SysUtils,
  variables, userHome, doubleLinkedList, jsonTools, userMessageDraft;

var
  draftsWindow: PGtkWidget;
  btnClose: PGtkWidget;
  lblDraftCount: PGtkWidget;
  treeView: PGtkWidget;
  listStore: PGtkListStore;

// ------------------------------
// Cerrar ventana y volver a Home
// ------------------------------
procedure OnCloseClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_widget_destroy(draftsWindow);
  ShowUserHomeWindow;
end;

// ------------------------------
// Doble clic en un borrador
// ------------------------------
procedure OnRowActivated(tree_view: PGtkTreeView; path: PGtkTreePath;
                         column: PGtkTreeViewColumn; data: gpointer); cdecl;
var
  model: PGtkTreeModel;
  iter: TGtkTreeIter;
  idCorreo, asunto, remitente: PChar;
  current: PMailNode;
begin
  model := gtk_tree_view_get_model(tree_view);

  if gtk_tree_model_get_iter(model, @iter, path) then
  begin
    gtk_tree_model_get(model, @iter,
      0, @idCorreo,
      1, @asunto,
      2, @remitente,
      -1);

    // Buscar en la lista doble el mail por ID
    current := DL_GetHead;
    while current <> nil do
    begin
      if StrPas(idCorreo) = current^.id then
      begin
        // Abrir mensaje borrador completo
        // 👇 Receiver no está en la lista doble, lo dejamos vacío
        ShowUserMessageDraft(current^.id, current^.sender,
                             '', current^.subject, current^.body);
        Break;
      end;
      current := current^.Next;
    end;
  end;
end;

// ------------------------------
// Cargar borradores a la tabla
// ------------------------------
procedure LoadDraftsToTreeView;
var
  iter: TGtkTreeIter;
  current: PMailNode;
  draftCount: Integer;
begin
  gtk_list_store_clear(listStore);

  draftCount := 0;
  current := DL_GetHead;
  while current <> nil do
  begin
    gtk_list_store_append(listStore, @iter);
    gtk_list_store_set(listStore, @iter,
      0, PChar(current^.id),       // ID
      1, PChar(current^.subject),  // Asunto
      2, PChar(current^.sender),   // Remitente
      -1);

    Inc(draftCount);
    current := current^.Next;
  end;

  // Actualizar contador
  gtk_label_set_text(GTK_LABEL(lblDraftCount),
    PChar('Borradores: ' + IntToStr(draftCount)));
end;

// ------------------------------
// Recargar borradores
// ------------------------------
procedure ReloadDrafts;
begin
  LoadDraftsToTreeView;
end;

// ------------------------------
// Mostrar ventana de borradores
// ------------------------------
procedure ShowUserDraftsWindow;
var
  vbox, hboxTop, hboxBottom: PGtkWidget;
  scrolledWindow: PGtkWidget;
  col: PGtkTreeViewColumn;
  cell: PGtkCellRenderer;
begin
  gtk_init(@argc, @argv);

  // Ventana principal
  draftsWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(draftsWindow), 'Borradores');
  gtk_container_set_border_width(GTK_CONTAINER(draftsWindow), 10);
  gtk_window_set_default_size(GTK_WINDOW(draftsWindow), 600, 400);

  vbox := gtk_vbox_new(False, 5);
  gtk_container_add(GTK_CONTAINER(draftsWindow), vbox);

  // Barra superior con contador
  hboxTop := gtk_hbox_new(False, 5);
  lblDraftCount := gtk_label_new('Borradores: 0');
  gtk_box_pack_end(GTK_BOX(hboxTop), lblDraftCount, False, False, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hboxTop, False, False, 0);

  // Tabla
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
  col := gtk_tree_view_column_new_with_attributes('ID', cell, 'text', 0, nil);
  gtk_tree_view_column_set_min_width(col, 60);
  gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), col);

  cell := gtk_cell_renderer_text_new;
  col := gtk_tree_view_column_new_with_attributes('Asunto', cell, 'text', 1, nil);
  gtk_tree_view_column_set_min_width(col, 250);
  gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), col);

  cell := gtk_cell_renderer_text_new;
  col := gtk_tree_view_column_new_with_attributes('Remitente', cell, 'text', 2, nil);
  gtk_tree_view_column_set_min_width(col, 200);
  gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), col);

  // Evento doble clic
  g_signal_connect(treeView, 'row-activated', G_CALLBACK(@OnRowActivated), nil);

  // Barra inferior con botón Cerrar
  hboxBottom := gtk_hbox_new(False, 5);
  btnClose := gtk_button_new_with_label('Cerrar');
  gtk_widget_set_size_request(btnClose, 120, 40);
  g_signal_connect(btnClose, 'clicked', G_CALLBACK(@OnCloseClick), nil);
  gtk_box_pack_start(GTK_BOX(hboxBottom), btnClose, True, True, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hboxBottom, False, False, 0);

  // Cargar borradores desde JSON
  DL_ClearList;
  LoadDraftsFromJson(json_file_drafts, current_user_email);
  LoadDraftsToTreeView;

  // Mostrar todo
  gtk_widget_show_all(draftsWindow);
  g_signal_connect(draftsWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

  gtk_main;
end;

end.

