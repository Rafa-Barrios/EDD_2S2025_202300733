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

    // File JSON
    json_file_path: string      = '/home/rafa/Documents/EDD_2S2025_202300733/Fase1/test/users.json';
    json_file_contacts: string  = '/home/rafa/Documents/EDD_2S2025_202300733/Fase1/test/contactos.json';
    json_file_mails: string     = '/home/rafa/Documents/EDD_2S2025_202300733/Fase1/test/mails.json';
    json_file_trash: string     = '/home/rafa/Documents/EDD_2S2025_202300733/Fase1/test/trash.json';
    json_file_scheduled: string = '/home/rafa/Documents/EDD_2S2025_202300733/Fase1/test/scheduled.json'; 

    // Credential root user
    root_user_email: string    = 'root';
    root_user_password: string = 'root';

implementation

end.