﻿unit main;

interface
{$WARN UNIT_PLATFORM OFF}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Types,
  System.IOUtils;

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
    FSL: TStringList;
    procedure SetDir(const Value: String);
    function GetDir: String;
    function GetFileList(const DirPath: string; const SearchPattern: string; const SearchOption: TSearchOption): TStringDynArray;
    procedure ConvertFiles(const FilePathList: TStringDynArray);
    procedure ConvertFile(const FilePath: String);
  public
    property SourceDir: String read GetDir write SetDir;
  end;

var
  fmPasAndDfmtoUTF8: TfmPasAndDfmtoUTF8;

implementation

uses
  FileCtrl, System.StrUtils;
{$R *.dfm}

procedure TfmPasAndDfmtoUTF8.bnSelectDirClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := SourceDir;
  if SelectDirectory(Dir, [], 0) then
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
var
  FileList: TStringDynArray;
begin
  FileList := GetFileList(SourceDir, '*.pas, *.dfm', TSearchOption.soAllDirectories);
  ConvertFiles(FileList);
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

function TfmPasAndDfmtoUTF8.GetFileList(const DirPath: string; const SearchPattern: string; const SearchOption: TSearchOption): TStringDynArray;
var
  FileSearchPattern: TStringDynArray;
  i: Integer;
  SList: TStringList;
  CurPattern: string;
begin
  SList := TStringList.Create;
  SList.Clear;
  FileSearchPattern := SplitString(SearchPattern, ';,');
  for i := 0 to Length(FileSearchPattern)-1 do
  begin
    CurPattern := Trim(FileSearchPattern[i]);
    SList.AddStrings(TArray<String>(TDirectory.GetFiles(DirPath, CurPattern, SearchOption)));
  end;
  Result := TStringDynArray(SList.ToStringArray);
end;

procedure TfmPasAndDfmtoUTF8.SetDir(const Value: String);
begin
  eDir.Text := Value;
end;

end.
