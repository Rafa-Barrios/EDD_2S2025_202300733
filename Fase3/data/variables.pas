unit variables;

interface

uses
    circularLinkedList,
    doubleLinkedList;

var
    // Current User
    current_user_id: string;
    current_user_name: string; 
    current_user_email: string;
    current_user_username: string;
    current_user_phone: string; 
    current_user_password: string;
    current_user_contacts: PCNode = nil;
    current_user_mails: PMailNode = nil; 
    current_user_trash: PMailNode = nil;     // Correos en papelera
    current_user_favorites: PMailNode = nil; 
    current_user_drafts: PMailNode = nil;

    // File JSON
    json_file_path: string      = '/home/rafa/Documents/EDD_2S2025_202300733/Fase3/test/users.json';
    json_file_contacts: string  = '/home/rafa/Documents/EDD_2S2025_202300733/Fase3/test/contactos.json';
    json_file_mails: string     = '/home/rafa/Documents/EDD_2S2025_202300733/Fase3/test/mails.json';
    json_file_trash: string     = '/home/rafa/Documents/EDD_2S2025_202300733/Fase3/test/trash.json';
    json_file_favorites: string  = '/home/rafa/Documents/EDD_2S2025_202300733/Fase3/test/favorites.json';
    json_file_scheduled: string = '/home/rafa/Documents/EDD_2S2025_202300733/Fase3/test/scheduled.json'; 
    json_file_drafts: string      = '/home/rafa/Documents/EDD_2S2025_202300733/Fase3/test/drafts.json';  // <-- nueva variable
    json_file_communities: string = '/home/rafa/Documents/EDD_2S2025_202300733/Fase3/test/com.json';  // <-- nueva variable
    json_file_login_control: string = '/home/rafa/Documents/EDD_2S2025_202300733/Fase3/test/loginControl.json';


    // Credential root user
    root_user_email: string    = '1';
    root_user_password: string = '1';

implementation

end.