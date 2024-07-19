unit uPesquisarRelacionamentos;
{
Criado por: Rodrigo Castro Eleotério
Data: 27/05/2013
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, BufDataset, Grids, DBGrids, StdCtrls,
  uERNotationsCore;

type

  { TFormPesquisarRelacionamentos }

  TFormPesquisarRelacionamentos = class(TForm)
    cds: TBufDataset;
    DBGrid1: TDBGrid;
    Ds: TDataSource;
    btAdd: TButton;
    Label1: TLabel;
    rbTabela: TRadioButton;
    rbOwner: TRadioButton;
    Label2: TLabel;
    edNome: TEdit;
    labQtde: TLabel;
    procedure btAddClick(Sender: TObject);
    procedure rbTabelaClick(Sender: TObject);
    procedure edNomeChange(Sender: TObject);
  private
    { Private declarations }
    //query: TADOQuery;
    procedure OrdenarTabelas;
  public
    { Public declarations }
    class procedure ObterTabelasFilhas(EntityPai: TEntity);
    class procedure ObterTabelasPai(EntityFilha: TEntity);
  end;

var
  FormPesquisarRelacionamentos: TFormPesquisarRelacionamentos;

implementation

uses uObterMetaDados, uPrincipal;

{$R *.lfm}

{ TFormDevendarRelacionamentos }

procedure TFormPesquisarRelacionamentos.btAddClick(Sender: TObject);
begin
  FormPrincipal.DiagramaManager.EntityContainerCorrente.AddEntity(Ds.DataSet.Fields[0].AsString, Ds.DataSet.Fields[1].AsString, 3, 3, False);
end;

procedure TFormPesquisarRelacionamentos.edNomeChange(Sender: TObject);
begin
  cds.Locate('tabela', edNome.Text, [loPartialKey])
end;

class procedure TFormPesquisarRelacionamentos.ObterTabelasFilhas(EntityPai: TEntity);
begin

  Application.CreateForm(TFormPesquisarRelacionamentos, FormPesquisarRelacionamentos);
  with FormPesquisarRelacionamentos do
  begin
    Caption := 'Buscando tabelas filhas de ' + EntityPai.SchemaOwner + '.' + EntityPai.NomeTabela;
    TObterMetaDados.ObterTabelasRelacionadasFilhas(EntityPai.SchemaOwner, EntityPai.PrimaryKeyConstraintName, EntityPai.NomeTabela, cds);
    OrdenarTabelas;
    Show;
  end;

end;

class procedure TFormPesquisarRelacionamentos.ObterTabelasPai(
  EntityFilha: TEntity);
begin
  Application.CreateForm(TFormPesquisarRelacionamentos, FormPesquisarRelacionamentos);
  with FormPesquisarRelacionamentos do
  begin
    Caption := 'Buscando tabelas pais de ' + EntityFilha.SchemaOwner + '.' + EntityFilha.NomeTabela;
    TObterMetaDados.ObterTabelasRelacionadasPai(EntityFilha.SchemaOwner, EntityFilha.NomeTabela, cds);
    OrdenarTabelas;
    Show;
  end;
end;

procedure TFormPesquisarRelacionamentos.OrdenarTabelas;
begin
  if rbOwner.Checked then
  begin
    cds.IndexFieldNames := 'Owner';
  end else if rbTabela.Checked then
  begin
    cds.IndexFieldNames := 'Tabela';
  end;
  labQtde.Caption := IntToStr(cds.RecordCount) + ' tabelas encontadas';
end;

procedure TFormPesquisarRelacionamentos.rbTabelaClick(Sender: TObject);
begin
  OrdenarTabelas;
end;

end.
