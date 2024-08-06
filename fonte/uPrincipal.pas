unit uPrincipal;
{
Criado por: Rodrigo Castro Eleotério
Data: 24/05/2013
}

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Menus, StdCtrls, Buttons,
  uFeaturesHandler, ComCtrls, IniFiles;

type
  TFormPrincipal = class(TForm)
    MainMenu1: TMainMenu;
    Arquivo11: TMenuItem;
    pnEntidades: TPanel;
    eladeTestes1: TMenuItem;
    SalvarModelo1: TMenuItem;
    AbrirModelo1: TMenuItem;
    AdicionarEntidade1: TMenuItem;
    Abrirdiagrama1: TMenuItem;
    Diagrama1: TMenuItem;
    Ajuda1: TMenuItem;
    Sobre1: TMenuItem;
    Novo1: TMenuItem;
    Fechar1: TMenuItem;
    N1: TMenuItem;
    Sair1: TMenuItem;
    StatusBar1: TStatusBar;
    Remover1: TMenuItem;
    NovoDiagrama1: TMenuItem;
    Renomear1: TMenuItem;
    ConfigurarConexo1: TMenuItem;
    N2: TMenuItem;
    Memo1: TMemo;
    ExportarparaImagem1: TMenuItem;
    Janelas1: TMenuItem;
    Ferramentas1: TMenuItem;
    PesquisarObjetos1: TMenuItem;
    MoverObjetos1: TMenuItem;
    procedure eladeTestes1Click(Sender: TObject);
    procedure AdicionarEntidade1Click(Sender: TObject);
    procedure SalvarModelo1Click(Sender: TObject);
    procedure AbrirModelo1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Abrirdiagrama1Click(Sender: TObject);
    procedure Novo1Click(Sender: TObject);
    procedure Fechar1Click(Sender: TObject);
    procedure Sair1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Remover1Click(Sender: TObject);
    procedure NovoDiagrama1Click(Sender: TObject);
    procedure Renomear1Click(Sender: TObject);
    procedure Sobre1Click(Sender: TObject);
    procedure ConfigurarConexo1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ExportarparaImagem1Click(Sender: TObject);
    procedure PesquisarObjetos1Click(Sender: TObject);
    procedure MoverObjetos1Click(Sender: TObject);
  private
    { Private declarations }
    FCaptionInicial: string;
    FFeaturesHanlder: TFeaturesHandler;
    //
    procedure HabilitarMenusContexto(Sender: TObject);
  public
    { Public declarations }
    procedure SetarAtividadeStatusPanelBar(texto: string);
    //
    property FeaturesHandler: TFeaturesHandler read FFeaturesHanlder;
  end;

var
  FormPrincipal: TFormPrincipal;

implementation

uses Unit2, uVariaveisGlobais, uPesquisarDiagramas, uSobre, uConfigConexao,
  uPesquisarTabelas, uAppFile, uMoverObjetos;

{$R *.lfm}

procedure TFormPrincipal.Abrirdiagrama1Click(Sender: TObject);
begin
  TFormPesquisarDiagramas.PesquiarDiagramas(FFeaturesHanlder.BuffDiagrams, FFeaturesHanlder);
end;

procedure TFormPrincipal.AdicionarEntidade1Click(Sender: TObject);
begin
  TFormPesquisarTabelas.PesquisarObjetos(True);
end;

procedure TFormPrincipal.ConfigurarConexo1Click(Sender: TObject);
begin
  TFormConfigConexao.ConfigConexao;
end;

procedure TFormPrincipal.AbrirModelo1Click(Sender: TObject);
begin
  if FFeaturesHanlder.HasFileToSave then
  begin
    {if  Application.MessageBox('Deseja salvar as alterações do Modelo atual?', 'ATENÇÃO',
      MB_YESNO + MB_ICONQUESTION) = IDYES then
    begin
      if FFeaturesHanlder.SaveFile then
      begin
        FFeaturesHanlder.CloseFile;
        FFeaturesHanlder.OpenFile;
      end;
    end
    else
    begin  }
      FFeaturesHanlder.CloseFile;
      FFeaturesHanlder.OpenFile('');
    //end;
  end
  else
    FFeaturesHanlder.OpenFile('');
end;

procedure TFormPrincipal.eladeTestes1Click(Sender: TObject);
begin
  form2.ShowModal;
end;

procedure TFormPrincipal.ExportarparaImagem1Click(Sender: TObject);
begin
  FFeaturesHanlder.CurrentDiagram.ScreenShot;
end;

procedure TFormPrincipal.Fechar1Click(Sender: TObject);
begin
  // o menu fechar só fica ativo se tiver algo para ser fechado
  {if  Application.MessageBox('Deseja salvar as alterações do Modelo atual?', 'ATENÇÃO',
    MB_YESNO + MB_ICONQUESTION) = IDYES then
  begin
    if FFeaturesHanlder.SaveFile then
      FFeaturesHanlder.CloseFile;
  end
  else  }
    FFeaturesHanlder.CloseFile;
end;

procedure TFormPrincipal.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caNone;

  if Application.MessageBox('Deseja encerrar o DB-Data?', 'Atenção', MB_YESNO +
    MB_ICONQUESTION) = IDYES then
  begin
    if FFeaturesHanlder.HasFileToSave then
      FFeaturesHanlder.CloseFile;
    Action := caFree;
  end;

  //if FFeaturesHanlder.HasFileToSave then
  //begin
    {if  Application.MessageBox('Deseja salvar as alterações do Modelo atual?', 'ATENÇÃO',
      MB_YESNO + MB_ICONQUESTION) = IDYES then
    begin
      if FFeaturesHanlder.SaveFile then
      begin
        FFeaturesHanlder.CloseFile;
        Action := caFree;
      end;
    end
    else
    begin   }
      //FFeaturesHanlder.CloseFile;
      {Action := caFree;  }
    //end;
  {end
  else
    Action := caFree; }
end;

procedure TFormPrincipal.FormDestroy(Sender: TObject);
begin
  //IniFile.Free;
  FFeaturesHanlder.Free;
end;

procedure TFormPrincipal.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //if (Key = VK_UP) or (Key = VK_DOWN) or (Key = VK_LEFT) or (Key = VK_RIGHT) then
  //  if (not (Screen.ActiveControl is TCustomEdit)) and (not (Screen.ActiveControl is TButtonControl)) then
  //    ShowMessage('scrllbox');
end;

procedure TFormPrincipal.FormShow(Sender: TObject);
begin
  // cria o ini de configurações
  //IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'DB-DataConf.ini');

  FCaptionInicial := Caption;

  FFeaturesHanlder := TFeaturesHandler.Create(Self);
  // seta o menu onde os diagramas abertos ficarão em lista
  FFeaturesHanlder.MenuItemOpenedFeatures := Janelas1;

  // se tiver um primeiro parâmetro, tenta abri-lo como um arquivo de diagrama
  if (ParamStr(1) <> '') and IsDBDataFile(ParamStr(1)) then
    FFeaturesHanlder.OpenFile(ParamStr(1))
  else
    // carrega o aplicativo com um modelo em branco para ser usado
    FFeaturesHanlder.NewFile;
    
  FFeaturesHanlder.OnChangeFeatureState := HabilitarMenusContexto;
  // chama a primeira vez manualmente
  HabilitarMenusContexto(Sender);

  MemoLog := Memo1;

  // ganhar tempo em debug
  FFeaturesHanlder.OpenFile('C:\Users\rceleoterio\Documents\sistemas hc.dbdata');
  FFeaturesHanlder.OpenEntityContainer('{6BB6AC5E-6013-433E-926E-BBF4962CDA66}');
end;

procedure TFormPrincipal.HabilitarMenusContexto(Sender: TObject);
begin
  // define os menus que devem estar habilitados
  Fechar1.Enabled := FFeaturesHanlder.HasFileToSave;
  SalvarModelo1.Enabled := FFeaturesHanlder.HasFileToSave;
  ConfigurarConexo1.Enabled := FFeaturesHanlder.HasFileToSave;;
  AdicionarEntidade1.Enabled := FFeaturesHanlder.CurrentDiagram <> nil;
  Abrirdiagrama1.Enabled := FFeaturesHanlder.HasFileToSave;
  Remover1.Enabled := FFeaturesHanlder.CurrentDiagram <> nil;
  NovoDiagrama1.Enabled := FFeaturesHanlder.HasFileToSave;
  Renomear1.Enabled := FFeaturesHanlder.CurrentDiagram <> nil;
  ExportarparaImagem1.Enabled := FFeaturesHanlder.CurrentDiagram <> nil;
  MoverObjetos1.Enabled := FFeaturesHanlder.CurrentDiagram <> nil;

  if FFeaturesHanlder.HasFileToSave then
  begin
    if FFeaturesHanlder.OpenedFile <> '' then
      Caption := FCaptionInicial + ' [' + FFeaturesHanlder.OpenedFile + ']'
    else
      Caption := FCaptionInicial + ' [Modelo sem nome]';
  end;

  // define o nome do diagrama aberto
  if FFeaturesHanlder.CurrentDiagram <> nil then
    StatusBar1.Panels[0].Text := 'Diagrama: ' + FFeaturesHanlder.CurrentDiagram.Titulo
  else
    StatusBar1.Panels[0].Text := 'Nenhum diagrama aberto';
end;

procedure TFormPrincipal.MoverObjetos1Click(Sender: TObject);
begin
  TFormMoverObjetos.MoverObjetos;
end;

procedure TFormPrincipal.Novo1Click(Sender: TObject);
begin
  if FFeaturesHanlder.HasFileToSave then
  begin
    {if  Application.MessageBox('Deseja salvar as alterações do Modelo atual?', 'ATENÇÃO',
      MB_YESNO + MB_ICONQUESTION) = IDYES then
    begin
      if FFeaturesHanlder.SaveFile then
      begin
        FFeaturesHanlder.CloseFile;
        FFeaturesHanlder.NewFile;
      end;
    end
    else
    begin     }
      FFeaturesHanlder.CloseFile;
      FFeaturesHanlder.NewFile;
    //end;
  end
  else
    FFeaturesHanlder.NewFile;
end;

procedure TFormPrincipal.NovoDiagrama1Click(Sender: TObject);
begin
  FFeaturesHanlder.NewDiagramER;
end;

procedure TFormPrincipal.PesquisarObjetos1Click(Sender: TObject);
begin
  TFormPesquisarTabelas.PesquisarObjetos(False);
end;

procedure TFormPrincipal.Remover1Click(Sender: TObject);
begin
  if Application.MessageBox('Deseja mesmo excluir o Diagrama selecionado?', 'ATENÇÃO', MB_YESNO + MB_ICONQUESTION) = IDYES then
    FFeaturesHanlder.RemoveDiagramER(FFeaturesHanlder.CurrentDiagram.DiagramaId);

end;

procedure TFormPrincipal.Renomear1Click(Sender: TObject);
begin
  FFeaturesHanlder.RenameDiagramER(FFeaturesHanlder.CurrentDiagram.DiagramaId);
end;

procedure TFormPrincipal.Sair1Click(Sender: TObject);
begin
  Close;
end;

procedure TFormPrincipal.SalvarModelo1Click(Sender: TObject);
begin
  // o menu salvar só fica ativo se tiver modelo aberto para ser salvo
  FFeaturesHanlder.SaveFile;
end;

procedure TFormPrincipal.SetarAtividadeStatusPanelBar(texto: string);
begin
  StatusBar1.Panels[1].Text := texto;
  StatusBar1.Repaint;
end;

procedure TFormPrincipal.Sobre1Click(Sender: TObject);
begin
  TFormSobre.Sobre;
end;

end.
