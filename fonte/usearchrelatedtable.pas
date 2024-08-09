unit uSearchRelatedTable;

{
Criado por: Rodrigo Castro Eleotério
Data: 27/05/2013
}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, DB,
  BufDataset, DBGrids, StdCtrls, uERNotationsCore;

type

  { TFormSearchRelatedTable }

  TFormSearchRelatedTable = class(TForm)
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
  FormSearchRelatedTable: TFormSearchRelatedTable;

implementation

uses uObterMetaDados, uPrincipal;

{$R *.lfm}

{ TFormSearchRelatedTable }

procedure TFormSearchRelatedTable.btAddClick(Sender: TObject);
begin
  FormPrincipal.FeaturesHandler.CurrentDiagram.AddEntity(Ds.DataSet.Fields[0].AsString, Ds.DataSet.Fields[1].AsString, 3, 3, False);
end;

procedure TFormSearchRelatedTable.edNomeChange(Sender: TObject);
begin
  cds.Locate('tabela', edNome.Text, [loPartialKey])
end;

class procedure TFormSearchRelatedTable.ObterTabelasFilhas(EntityPai: TEntity);
begin

  Application.CreateForm(TFormSearchRelatedTable, FormSearchRelatedTable);
  with FormSearchRelatedTable do
  begin
    Caption := 'Buscando tabelas filhas de ' + EntityPai.SchemaOwner + '.' + EntityPai.NomeTabela;
    Screen.Cursor := crHourGlass;
    TObterMetaDados.ObterTabelasRelacionadasFilhas(EntityPai.SchemaOwner, EntityPai.PrimaryKeyConstraintName, EntityPai.NomeTabela, cds);
    Screen.Cursor := crDefault;
    OrdenarTabelas;
    Show;
  end;

end;

class procedure TFormSearchRelatedTable.ObterTabelasPai(
  EntityFilha: TEntity);
begin
  Application.CreateForm(TFormSearchRelatedTable, FormSearchRelatedTable);
  with FormSearchRelatedTable do
  begin
    Caption := 'Buscando tabelas pais de ' + EntityFilha.SchemaOwner + '.' + EntityFilha.NomeTabela;
    Screen.Cursor := crHourGlass;
    TObterMetaDados.ObterTabelasRelacionadasPai(EntityFilha.SchemaOwner, EntityFilha.NomeTabela, cds);
    Screen.Cursor := crDefault;
    OrdenarTabelas;
    Show;
  end;
end;

procedure TFormSearchRelatedTable.OrdenarTabelas;
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

procedure TFormSearchRelatedTable.rbTabelaClick(Sender: TObject);
begin
  OrdenarTabelas;
end;

end.
