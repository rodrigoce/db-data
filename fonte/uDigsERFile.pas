
{**********************************************************************************************}

{                                       XML Data Binding                                       }

{**********************************************************************************************}

unit uDigsERFile;

{$MODE Delphi}

{
Criado por: Rodrigo Castro Eleotério
Data: 14/10/2013
}

interface

uses SysUtils, Contnrs, Generics.Collections;

type

  { Forward Decls }

  TFileContent = class;
  TDiagrama = class;
  TEntidade = class;
  TRelacionamento = class;

  { Types }

  TFileContent = class
  private
    FVersao: string;
    FDiagramas: TObjectList<TDiagrama>;
  public
    property Versao: string read FVersao write FVersao;
    property Diagramas: TObjectList<TDiagrama> read FDiagramas write FDiagramas;
  end;

  TDiagrama = class
  private
    FTitulo: string;
    FID: string;
    FEntidades: TObjectList<TEntidade>;
    FRelacionamentos: TObjectList<TRelacionamento>;
  public
    property Titulo: string read FTitulo write FTitulo;
    property ID: string read FID write FID;
    property Entidades: TObjectList<TEntidade> read FEntidades write FEntidades;
    property Relacionamentos: TObjectList<TRelacionamento> read FRelacionamentos write FRelacionamentos;
    constructor Create(ATitle: string; AID: string);
    destructor Destroy;
  end;

  TEntidade = class
  private
    FTop: integer;
    FLeft: integer;
    FOwner: string;
    FTabela: string;
    FTodosOsCampos: boolean;

  public
    property Top: integer read FTop write FTop;
    property Left: integer read FLeft write FLeft;
    property Owner: string read FOwner write FOwner;
    property Tabela: string read FTabela write FTabela;
    property TodosOsCampos: boolean read FTodosOsCampos write FTodosOsCampos;

    constructor Create(ATop: integer; ALeft: integer; AOwner: string;
      ATabela: string; ATodosOsCampos: boolean);
    destructor Destroy;
  end;

  TRelacionamento = class
  private
    FNomeCaminho: string;
    FDistanciaLateral: integer;
    FOwner: string;
    FConstraintName: string;

  public
    property NomeCaminho: string read FNomeCaminho write FNomeCaminho;
    property DistanciaLateral: integer read FDistanciaLateral
      write FDistanciaLateral;
    property Owner: string read FOwner write FOwner;
    property ConstraintName: string read FConstraintName write FConstraintName;

    constructor Create(ANomeCaminho: string; ADistanciaLateral: integer;
      AOwner: string; AConstraintName: string);
    destructor Destroy;
  end;


  { Global Functions }

(*function Getdiagramas(Doc: IXMLDocument): IXMLDiagramasType;
function Loaddiagramas(const FileName: WideString): IXMLDiagramasType;
function Newdiagramas: IXMLDiagramasType;        *)
function IsDigsERFile(fileName: string): boolean;

const
  TargetNamespace = '';

implementation

{ Global Functions }

(*function Getdiagramas(Doc: IXMLDocument): IXMLDiagramasType;
begin
  Result := Doc.GetDocBinding('diagramas', TXMLDiagramasType, TargetNamespace) as IXMLDiagramasType;
end;

function Loaddiagramas(const FileName: WideString): IXMLDiagramasType;
begin
  Result := LoadXMLDocument(FileName).GetDocBinding('diagramas', TXMLDiagramasType, TargetNamespace) as IXMLDiagramasType;
end;

function Newdiagramas: IXMLDiagramasType;
begin
  Result := NewXMLDocument.GetDocBinding('diagramas', TXMLDiagramasType, TargetNamespace) as IXMLDiagramasType;
end; *)

function IsDigsERFile(fileName: string): boolean;
begin
  Result := False;
  if LowerCase(ExtractFileExt(fileName)) = '.dger' then
    if FileExists(fileName) then
      Result := True;
end;

{ TDiagrama }

constructor TDiagrama.Create(ATitle: string; AID: string);
begin
  FTitulo := ATitle;
  FID := AID;
  //FEntidades := [];
  //FRelacionamentos := [];
end;

destructor TDiagrama.Destroy;
begin
  FreeAndNil(FEntidades);
  FreeAndNil(FRelacionamentos);
end;

{ TEntidade }

constructor TEntidade.Create(ATop: integer; ALeft: integer; AOwner: string;
  ATabela: string; ATodosOsCampos: boolean);
begin
  FTop := ATop;
  FLeft := ALeft;
  FOwner := AOwner;
  FTabela := ATabela;
  FTodosOsCampos := ATodosOsCampos;
end;

destructor TEntidade.Destroy;
begin
end;

{ TRelacionamento }

constructor TRelacionamento.Create(ANomeCaminho: string;
  ADistanciaLateral: integer; AOwner: string; AConstraintName: string);
begin
  FNomeCaminho := ANomeCaminho;
  FDistanciaLateral := ADistanciaLateral;
  FOwner := AOwner;
  FConstraintName := AConstraintName;
end;

destructor TRelacionamento.Destroy;
begin
end;

end.
