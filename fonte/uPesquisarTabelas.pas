unit uPesquisarTabelas;
{
Criado por: Rodrigo Castro Eleotério
Data: 27/11/2013
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, DBGrids, ExtCtrls, DB, BufDataset, Clipbrd;

type

  { TFormPesquisarTabelas }

  TFormPesquisarTabelas = class(TForm)
    cdsTabelas: TBufDataset;
    Label1: TLabel;
    edOwner: TComboBox;
    btObterOwner: TButton;
    rgTipoPesquisa: TRadioGroup;
    Label2: TLabel;
    edTabela: TEdit;
    Shape1: TShape;
    btPesquisar: TButton;
    dbGrid: TDBGrid;
    Shape2: TShape;
    btAdicionarNoDiagrama: TButton;
    btFechar: TButton;
    Ds: TDataSource;
    labQtde: TLabel;
    procedure btFecharClick(Sender: TObject);
    procedure btObterOwnerClick(Sender: TObject);
    procedure btPesquisarClick(Sender: TObject);
    procedure btAdicionarNoDiagramaClick(Sender: TObject);
    procedure dbGridKeyPress(Sender: TObject; var Key: Char);
    procedure edTabelaKeyPress(Sender: TObject; var Key: Char);
    procedure dbGridDblClick(Sender: TObject);
    procedure edOwnerKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    FTipoTabela: Boolean;
  public
    { Public declarations }
    class procedure PesquisarObjetos(tipoTabela: Boolean);
  end;

var
  FormPesquisarTabelas: TFormPesquisarTabelas;

implementation

uses uVariaveisGlobais, uObterMetaDados, uPrincipal;

{$R *.lfm}

procedure TFormPesquisarTabelas.btAdicionarNoDiagramaClick(Sender: TObject);
begin
  if (cdsTabelas.Active) and  (cdsTabelas.RecordCount > 0) then
      FormPrincipal.DiagramaManager.CurrentDiagram.AddEntity(cdsTabelas.FieldByName('owner').AsString, cdsTabelas.FieldByName('object_name').AsString, 3, 3, False)
  else
    Application.MessageBox('Nenhuma Tabela foi selecionada!', 'ATENÇÃO', MB_OK + MB_ICONWARNING);
end;

procedure TFormPesquisarTabelas.btFecharClick(Sender: TObject);
begin
  Close;
end;

procedure TFormPesquisarTabelas.btObterOwnerClick(Sender: TObject);
begin
  if not Assigned(CacheDeOwners) then
    TObterMetaDados.ObterListaDeOwners;

  edOwner.Items.AddStrings(CacheDeOwners)
end;

procedure TFormPesquisarTabelas.btPesquisarClick(Sender: TObject);
var
  count: Integer;
begin
  cdsTabelas.Close;
  if edTabela.GetTextLen > 0 then
  begin
    TObterMetaDados.PesquisarObjetos(edOwner.Text, edTabela.Text, rgTipoPesquisa.ItemIndex, FTipoTabela, cdsTabelas);
  end
  else
    Application.MessageBox('Você não informou o nome da tabela a ser pesquisado!',
      'ATENÇÃO', MB_OK + MB_ICONWARNING);

  count := 0;
  if cdsTabelas.Active then
    count := cdsTabelas.RecordCount;

  labQtde.Caption := IntToStr(count) + ' tabelas encontradas';
end;

procedure TFormPesquisarTabelas.dbGridDblClick(Sender: TObject);
begin
  if btAdicionarNoDiagrama.Visible then
    btAdicionarNoDiagrama.Click;
end;

procedure TFormPesquisarTabelas.dbGridKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = ^C then
  begin
    Clipboard.AsText := cdsTabelas.FieldByName('owner').AsString + '.' +
      cdsTabelas.FieldByName('object_name').AsString;
  end;
end;

procedure TFormPesquisarTabelas.edOwnerKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    btObterOwner.Click;
end;

procedure TFormPesquisarTabelas.edTabelaKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then
    btPesquisar.Click;
end;

class procedure TFormPesquisarTabelas.PesquisarObjetos(tipoTabela: Boolean);
begin
  Application.CreateForm(TFormPesquisarTabelas, FormPesquisarTabelas);
  with FormPesquisarTabelas do
  begin
    if Assigned(CacheDeOwners) then
      edOwner.Items.AddStrings(CacheDeOwners);

    FTipoTabela := tipoTabela;
    if not tipoTabela then
    begin
      btAdicionarNoDiagrama.Enabled := False;
    end;

    ShowModal;
    Free;
  end;
end;

end.
