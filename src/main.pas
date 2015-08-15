unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Types;

type
  TfmPasAndDfmtoUTF8 = class(TForm)
    eDir: TEdit;
    bnSelectDir: TButton;
    bnConvert: TButton;
    procedure bnSelectDirClick(Sender: TObject);
    procedure bnConvertClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FPasFileList: TStringDynArray;
    FDfmFileList: TStringDynArray;
    FSL: TStringList;
    procedure SetDir(const Value: String);
    function GetDir: String;
    procedure GetFileLists;
    procedure ConvertFiles(const FilePathList: TStringDynArray);
    procedure ConvertFile(const FilePath: String);
  public
    property SourceDir: String read GetDir write SetDir;
  end;

var
  fmPasAndDfmtoUTF8: TfmPasAndDfmtoUTF8;

implementation

uses
  FileCtrl, System.IOUtils;
{$R *.dfm}

procedure TfmPasAndDfmtoUTF8.bnSelectDirClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := SourceDir;
  SelectDirectory(Dir, [], 0);
  SourceDir := Dir;
end;

procedure TfmPasAndDfmtoUTF8.ConvertFile(const FilePath: String);
begin
  FSL.LoadFromFile(FilePath);

  if FSL.Encoding <> TEncoding.UTF8 then
    FSL.SaveToFile(FilePath, TEncoding.UTF8);
end;

procedure TfmPasAndDfmtoUTF8.ConvertFiles(const FilePathList: TStringDynArray);
var
  i: Integer;
begin
  for i := 0 to Length(FilePathList)-1 do
    ConvertFile(FilePathList[i]);
end;

procedure TfmPasAndDfmtoUTF8.bnConvertClick(Sender: TObject);
begin
  GetFileLists;
  ConvertFiles(FPasFileList);
  ConvertFiles(FDfmFileList);
  ShowMessage('Готово!');
end;

procedure TfmPasAndDfmtoUTF8.FormCreate(Sender: TObject);
begin
  SourceDir := GetCurrentDir;
  FSL := TStringList.Create;
end;

procedure TfmPasAndDfmtoUTF8.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSL);
end;

function TfmPasAndDfmtoUTF8.GetDir: String;
begin
  Result := eDir.Text;
end;

procedure TfmPasAndDfmtoUTF8.GetFileLists;
begin
  FPasFileList := TDirectory.GetFiles(SourceDir, '*.pas', TSearchOption.soAllDirectories);
  FDfmFileList := TDirectory.GetFiles(SourceDir, '*.dfm', TSearchOption.soAllDirectories);
end;

procedure TfmPasAndDfmtoUTF8.SetDir(const Value: String);
begin
  eDir.Text := Value;
end;

end.
