unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, DB, oracleconnection, Buttons, Menus,
  Grids, DBGrids;

type

  { TForm2 }

  TForm2 = class(TForm)
    OracleConnection1: TOracleConnection;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Shape1: TShape;
    Shape2: TShape;
    Label1: TLabel;
    Label2: TLabel;
    Shape3: TShape;
    Shape4: TShape;
    Label3: TLabel;
    Label4: TLabel;
    datadatadada: TEdit;
    DataSource1: TDataSource;
    Label5: TLabel;
    Button1: TButton;
    Button2: TButton;
    DBGrid1: TDBGrid;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    ClientDataSet1Titulo: TStringField;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    MouseDownSpot : TPoint;
    Capturing : bool;
  end;
var
  Form2: TForm2;

implementation

{$R *.LFM}

procedure TForm2.Button1Click(Sender: TObject);
begin
 // ClientDataSet1.CreateDataSet;
 // ClientDataSet1.IndexFieldNames := 'a';

end;

procedure TForm2.Button2Click(Sender: TObject);
begin
//  ClientDataSet1.Append;
//  ClientDataSet1.Fields[0].Value := datadatadada.Text;
//  ClientDataSet1.Post;
end;

end.
