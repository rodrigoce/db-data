
{**********************************************************************************************}
{                                                                                              }
{                                       XML Data Binding                                       }
{                                                                                              }
{**********************************************************************************************}

unit uDigsERFile;

{$MODE Delphi}

{
Criado por: Rodrigo Castro Eleotério
Data: 14/10/2013
}

interface

uses {xmldom, XMLDoc, XMLIntf,} SysUtils;

type

{ Forward Decls }

  IXMLDiagramasType = interface;
  IXMLDiagramaType = interface;
  IXMLEntidadesType = interface;
  IXMLEntidadeType = interface;
  IXMLRelacionamentosType = interface;
  IXMLRelacionamentoType = interface;

  //IXMLQueries = interface;
  //IXMLQuery = interface;

{ IXMLDiagramasType }

  IXMLDiagramasType = interface{(IXMLNodeCollection)}
    ['{E357402E-D4BF-4703-988E-7439A1373AAD}']
    { Property Accessors }
    function Get_Versao: WideString;
    function Get_Diagrama(Index: Integer): IXMLDiagramaType;
    procedure Set_Versao(Value: WideString);
    { Methods & Properties }
    function Add: IXMLDiagramaType;
    function Insert(const Index: Integer): IXMLDiagramaType;
    property Versao: WideString read Get_Versao write Set_Versao;
    property Diagrama[Index: Integer]: IXMLDiagramaType read Get_Diagrama; default;
  end;


{ IXMLDiagramaType }

  IXMLDiagramaType = interface{(IXMLNode)}
    ['{A002DBDF-9859-4A86-B181-AC505832FEA4}']
    { Property Accessors }
    function Get_Titulo: WideString;
    function Get_Entidades: IXMLEntidadesType;
    function Get_Relacionamentos: IXMLRelacionamentosType;
    procedure Set_Titulo(Value: WideString);
    function Get_Id: WideString;
    procedure Set_Id(Value: WideString);
    { Methods & Properties }
    property Titulo: WideString read Get_Titulo write Set_Titulo;
    property Id: WideString read Get_Id write Set_Id;
    property Entidades: IXMLEntidadesType read Get_Entidades;
    property Relacionamentos: IXMLRelacionamentosType read Get_Relacionamentos;
  end;

{ IXMLEntidadesType }

  IXMLEntidadesType = interface{(IXMLNodeCollection)}
    ['{F9E0A4ED-EA9F-426B-80BF-D118557C3B90}']
    { Property Accessors }
    function Get_Entidade(Index: Integer): IXMLEntidadeType;
    { Methods & Properties }
    function Add: IXMLEntidadeType;
    function Insert(const Index: Integer): IXMLEntidadeType;
    property Entidade[Index: Integer]: IXMLEntidadeType read Get_Entidade; default;
  end;

{ IXMLEntidadeType }

  IXMLEntidadeType = interface{(IXMLNode)}
    ['{956E4060-6D8A-4730-9A45-FCB23CCB6ECE}']
    { Property Accessors }
    function Get_Top: Integer;
    function Get_Left: Integer;
    function Get_Owner: WideString;
    function Get_Tabela: WideString;
    function Get_TodosOsCampos: Boolean;
    procedure Set_Top(Value: Integer);
    procedure Set_Left(Value: Integer);
    procedure Set_Owner(Value: WideString);
    procedure Set_Tabela(Value: WideString);
    procedure Set_TodosOsCampos(Value: Boolean);
    { Methods & Properties }
    property Top: Integer read Get_Top write Set_Top;
    property Left: Integer read Get_Left write Set_Left;
    property Owner: WideString read Get_Owner write Set_Owner;
    property Tabela: WideString read Get_Tabela write Set_Tabela;
    property TodosOsCampos: Boolean read Get_TodosOsCampos write Set_TodosOsCampos;
  end;

{ IXMLRelacionamentosType }

  IXMLRelacionamentosType = interface{(IXMLNodeCollection)}
    ['{D22F371F-E869-40BB-9DA5-B718C5D4755D}']
    { Property Accessors }
    function Get_Relacionamento(Index: Integer): IXMLRelacionamentoType;
    { Methods & Properties }
    function Add: IXMLRelacionamentoType;
    function Insert(const Index: Integer): IXMLRelacionamentoType;
    property Relacionamento[Index: Integer]: IXMLRelacionamentoType read Get_Relacionamento; default;
  end;

{ IXMLRelacionamentoType }

  IXMLRelacionamentoType = interface{(IXMLNode)}
    ['{9E33ABC6-2F2D-4B9D-8D08-E67894491A6F}']
    { Property Accessors }
    function Get_NomeCaminho: WideString;
    function Get_DistanciaLateral: Integer;
    function Get_Owner: WideString;
    function Get_ConstraintName: WideString;
    procedure Set_NomeCaminho(Value: WideString);
    procedure Set_DistanciaLateral(Value: Integer);
    procedure Set_Onwer(Value: WideString);
    procedure Set_ConstraintName(Value: WideString);
    { Methods & Properties }
    property NomeCaminho: WideString read Get_NomeCaminho write Set_NomeCaminho;
    property DistanciaLateral: Integer read Get_DistanciaLateral write Set_DistanciaLateral;
    property Onwer: WideString read Get_Owner write Set_Onwer;
    property ConstraintName: WideString read Get_ConstraintName write Set_ConstraintName;
  end;

{ Forward Decls }

  {TXMLDiagramasType = class;
  TXMLDiagramaType = class;
  TXMLEntidadesType = class;
  TXMLEntidadeType = class;
  TXMLRelacionamentosType = class;
  TXMLRelacionamentoType = class;}

{ TXMLDiagramasType }

  (*TXMLDiagramasType = class(TXMLNodeCollection, IXMLDiagramasType)
  protected
    { IXMLDiagramasType }
    function Get_Versao: WideString;
    function Get_Diagrama(Index: Integer): IXMLDiagramaType;
    procedure Set_Versao(Value: WideString);
    function Add: IXMLDiagramaType;
    function Insert(const Index: Integer): IXMLDiagramaType;
  public
    procedure AfterConstruction; override;
  end;*)

{ TXMLDiagramaType }

  (*TXMLDiagramaType = class({TXMLNode,} IXMLDiagramaType)
  protected
    { IXMLDiagramaType }
    function Get_Titulo: WideString;
    function Get_Entidades: IXMLEntidadesType;
    function Get_Relacionamentos: IXMLRelacionamentosType;
    procedure Set_Titulo(Value: WideString);
    function Get_Id: WideString;
    procedure Set_Id(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;*)

{ TXMLEntidadesType }

  (*TXMLEntidadesType = class(TXMLNodeCollection, IXMLEntidadesType)
  protected
    { IXMLEntidadesType }
    function Get_Entidade(Index: Integer): IXMLEntidadeType;
    function Add: IXMLEntidadeType;
    function Insert(const Index: Integer): IXMLEntidadeType;
  public
    procedure AfterConstruction; override;
  end;*)

{ TXMLEntidadeType }

  (*TXMLEntidadeType = class(TXMLNode, IXMLEntidadeType)
  protected
    { IXMLEntidadeType }
    function Get_Top: Integer;
    function Get_Left: Integer;
    function Get_Owner: WideString;
    function Get_Tabela: WideString;
    function Get_TodosOsCampos: Boolean;
    procedure Set_Top(Value: Integer);
    procedure Set_Left(Value: Integer);
    procedure Set_Owner(Value: WideString);
    procedure Set_Tabela(Value: WideString);
    procedure Set_TodosOsCampos(Value: Boolean);
  end;*)

{ TXMLRelacionamentosType }

  (*TXMLRelacionamentosType = class(TXMLNodeCollection, IXMLRelacionamentosType)
  protected
    { IXMLRelacionamentosType }
    function Get_Relacionamento(Index: Integer): IXMLRelacionamentoType;
    function Add: IXMLRelacionamentoType;
    function Insert(const Index: Integer): IXMLRelacionamentoType;
  public
    procedure AfterConstruction; override;
  end;*)

{ TXMLRelacionamentoType }

  (*TXMLRelacionamentoType = class(TXMLNode, IXMLRelacionamentoType)
  protected
    { IXMLRelacionamentoType }
    function Get_NomeCaminho: WideString;
    function Get_DistanciaLateral: Integer;
    function Get_Owner: WideString;
    function Get_ConstraintName: WideString;
    procedure Set_NomeCaminho(Value: WideString);
    procedure Set_DistanciaLateral(Value: Integer);
    procedure Set_Onwer(Value: WideString);
    procedure Set_ConstraintName(Value: WideString);
  end;*)

{ Global Functions }

(*function Getdiagramas(Doc: IXMLDocument): IXMLDiagramasType;
function Loaddiagramas(const FileName: WideString): IXMLDiagramasType;
function Newdiagramas: IXMLDiagramasType;        *)
function IsDigsERFile(fileName: string): Boolean;

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

function IsDigsERFile(fileName: string): Boolean;
begin
  Result := False;         
  if LowerCase(ExtractFileExt(fileName)) = '.dger' then
    if FileExists(fileName) then
      Result := True;
end;
(*
{ TXMLDiagramasType }

procedure TXMLDiagramasType.AfterConstruction;
begin
  RegisterChildNode('diagrama', TXMLDiagramaType);
  ItemTag := 'diagrama';
  ItemInterface := IXMLDiagramaType;
  inherited;
end;

function TXMLDiagramasType.Get_Versao: WideString;
begin
  Result := AttributeNodes['versao'].Text;
end;

procedure TXMLDiagramasType.Set_Versao(Value: WideString);
begin
  SetAttribute('versao', Value);
end;

function TXMLDiagramasType.Get_Diagrama(Index: Integer): IXMLDiagramaType;
begin
  Result := List[Index] as IXMLDiagramaType;
end;

function TXMLDiagramasType.Add: IXMLDiagramaType;
begin
  Result := AddItem(-1) as IXMLDiagramaType;
end;

function TXMLDiagramasType.Insert(const Index: Integer): IXMLDiagramaType;
begin
  Result := AddItem(Index) as IXMLDiagramaType;
end;

{ TXMLDiagramaType }

procedure TXMLDiagramaType.AfterConstruction;
begin
  RegisterChildNode('entidades', TXMLEntidadesType);
  RegisterChildNode('relacionamentos', TXMLRelacionamentosType);
  inherited;
end;

function TXMLDiagramaType.Get_Titulo: WideString;
begin
  Result := AttributeNodes['titulo'].Text;
end;

procedure TXMLDiagramaType.Set_Id(Value: WideString);
begin
  SetAttribute('id', Value);
end;

procedure TXMLDiagramaType.Set_Titulo(Value: WideString);
begin
  SetAttribute('titulo', Value);
end;

function TXMLDiagramaType.Get_Entidades: IXMLEntidadesType;
begin
  Result := ChildNodes['entidades'] as IXMLEntidadesType;
end;

function TXMLDiagramaType.Get_Id: WideString;
begin
  Result := AttributeNodes['id'].Text;
end;

function TXMLDiagramaType.Get_Relacionamentos: IXMLRelacionamentosType;
begin
  Result := ChildNodes['relacionamentos'] as IXMLRelacionamentosType;
end;

{ TXMLEntidadesType }

procedure TXMLEntidadesType.AfterConstruction;
begin
  RegisterChildNode('entidade', TXMLEntidadeType);
  ItemTag := 'entidade';
  ItemInterface := IXMLEntidadeType;
  inherited;
end;

function TXMLEntidadesType.Get_Entidade(Index: Integer): IXMLEntidadeType;
begin
  Result := List[Index] as IXMLEntidadeType;
end;

function TXMLEntidadesType.Add: IXMLEntidadeType;
begin
  Result := AddItem(-1) as IXMLEntidadeType;
end;

function TXMLEntidadesType.Insert(const Index: Integer): IXMLEntidadeType;
begin
  Result := AddItem(Index) as IXMLEntidadeType;
end;

{ TXMLEntidadeType }

function TXMLEntidadeType.Get_TodosOsCampos: Boolean;
begin
  Result := AttributeNodes['todosOsCampos'].NodeValue;
end;

function TXMLEntidadeType.Get_Top: Integer;
begin
  Result := AttributeNodes['top'].NodeValue;
end;

procedure TXMLEntidadeType.Set_TodosOsCampos(Value: Boolean);
begin
  SetAttribute('todosOsCampos', Value);
end;

procedure TXMLEntidadeType.Set_Top(Value: Integer);
begin
  SetAttribute('top', Value);
end;

function TXMLEntidadeType.Get_Left: Integer;
begin
  Result := AttributeNodes['left'].NodeValue;
end;

procedure TXMLEntidadeType.Set_Left(Value: Integer);
begin
  SetAttribute('left', Value);
end;

function TXMLEntidadeType.Get_Owner: WideString;
begin
  Result := AttributeNodes['owner'].Text;
end;

procedure TXMLEntidadeType.Set_Owner(Value: WideString);
begin
  SetAttribute('owner', Value);
end;

function TXMLEntidadeType.Get_Tabela: WideString;
begin
  Result := AttributeNodes['tabela'].Text;
end;

procedure TXMLEntidadeType.Set_Tabela(Value: WideString);
begin
  SetAttribute('tabela', Value);
end;

{ TXMLRelacionamentosType }

function TXMLRelacionamentosType.Add: IXMLRelacionamentoType;
begin
  Result := AddItem(-1) as IXMLRelacionamentoType;
end;

procedure TXMLRelacionamentosType.AfterConstruction;
begin
  RegisterChildNode('relacionamento', TXMLRelacionamentoType);
  ItemTag := 'relacionamento';
  ItemInterface := IXMLRelacionamentoType;
  inherited;
end;

function TXMLRelacionamentosType.Get_Relacionamento(
  Index: Integer): IXMLRelacionamentoType;
begin
  Result := List[Index] as IXMLRelacionamentoType; 
end;

function TXMLRelacionamentosType.Insert(
  const Index: Integer): IXMLRelacionamentoType;
begin
  Result := AddItem(Index) as IXMLRelacionamentoType;
end;

{ TXMLRelacionamentoType }

function TXMLRelacionamentoType.Get_ConstraintName: WideString;
begin
  Result := AttributeNodes['constraintName'].Text;
end;

function TXMLRelacionamentoType.Get_DistanciaLateral: Integer;
begin
  Result := AttributeNodes['distanciaLateral'].NodeValue;
end;

function TXMLRelacionamentoType.Get_NomeCaminho: WideString;
begin
  Result := AttributeNodes['nomeCaminho'].Text;
end;

function TXMLRelacionamentoType.Get_Owner: WideString;
begin
  Result := AttributeNodes['owner'].Text;
end;

procedure TXMLRelacionamentoType.Set_ConstraintName(Value: WideString);
begin
  SetAttribute('constraintName', Value);
end;

procedure TXMLRelacionamentoType.Set_DistanciaLateral(Value: Integer);
begin
  SetAttribute('distanciaLateral', Value);
end;

procedure TXMLRelacionamentoType.Set_NomeCaminho(Value: WideString);
begin
  SetAttribute('nomeCaminho', Value);
end;

procedure TXMLRelacionamentoType.Set_Onwer(Value: WideString);
begin
  SetAttribute('owner', Value);
end;*)

end.
