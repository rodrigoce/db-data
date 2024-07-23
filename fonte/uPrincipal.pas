unit uPrincipal;
{
Criado por: Rodrigo Castro Eleotério
Data: 24/05/2013
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Menus, StdCtrls, Buttons,
  uDiagramaManager, ComCtrls, IniFiles;

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
    FDiagramaManager: TDiagramaManager;
    //
    procedure HabilitarMenusContexto(Sender: TObject);
  public
    { Public declarations }
    procedure SetarAtividadeStatusPanelBar(texto: string);
    //
    property DiagramaManager: TDiagramaManager read FDiagramaManager;
  end;

var
  FormPrincipal: TFormPrincipal;

implementation

uses Unit2, uVariaveisGlobais, uPesquisarDiagramas, uSobre, uConfigConexao,
  uPesquisarTabelas, uDigsERFile, uMoverObjetos;

{$R *.lfm}

procedure TFormPrincipal.Abrirdiagrama1Click(Sender: TObject);
begin
  TFormPesquisarDiagramas.PesquiarDiagramas(FDiagramaManager.CdsDiagramas, FDiagramaManager);
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
  if FDiagramaManager.TemModeloParaSalvar then
  begin
    {if  Application.MessageBox('Deseja salvar as alterações do Modelo atual?', 'ATENÇÃO',
      MB_YESNO + MB_ICONQUESTION) = IDYES then
    begin
      if FDiagramaManager.SaveModelo then
      begin
        FDiagramaManager.FecharModelo;
        FDiagramaManager.OpenModelo;
      end;
    end
    else
    begin  }
      FDiagramaManager.FecharModelo;
      FDiagramaManager.OpenModelo('');
    //end;
  end
  else
    FDiagramaManager.OpenModelo('');
end;

procedure TFormPrincipal.eladeTestes1Click(Sender: TObject);
begin
  form2.ShowModal;
end;

procedure TFormPrincipal.ExportarparaImagem1Click(Sender: TObject);
begin
  FDiagramaManager.EntityContainerCorrente.ScreenShot;
end;

procedure TFormPrincipal.Fechar1Click(Sender: TObject);
begin
  // o menu fechar só fica ativo se tiver algo para ser fechado
  {if  Application.MessageBox('Deseja salvar as alterações do Modelo atual?', 'ATENÇÃO',
    MB_YESNO + MB_ICONQUESTION) = IDYES then
  begin
    if FDiagramaManager.SaveModelo then
      FDiagramaManager.FecharModelo;
  end
  else  }
    FDiagramaManager.FecharModelo;
end;

procedure TFormPrincipal.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caNone;

  if Application.MessageBox('Deseja encerrar o DigsER?', 'Atenção', MB_YESNO + 
    MB_ICONQUESTION) = IDYES then
  begin
    if FDiagramaManager.TemModeloParaSalvar then
      FDiagramaManager.FecharModelo;
    Action := caFree;
  end;

  //if FDiagramaManager.TemModeloParaSalvar then
  //begin
    {if  Application.MessageBox('Deseja salvar as alterações do Modelo atual?', 'ATENÇÃO',
      MB_YESNO + MB_ICONQUESTION) = IDYES then
    begin
      if FDiagramaManager.SaveModelo then
      begin
        FDiagramaManager.FecharModelo;
        Action := caFree;
      end;
    end
    else
    begin   }
      //FDiagramaManager.FecharModelo;
      {Action := caFree;  }
    //end;
  {end
  else
    Action := caFree; }
end;

procedure TFormPrincipal.FormDestroy(Sender: TObject);
begin
  IniFile.Free;
  FDiagramaManager.Free;
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
  IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'DigsERConf.ini');

  FDiagramaManager := TDiagramaManager.Create(Self);
  // seta o menu onde os diagramas abertos ficarão em lista
  FDiagramaManager.MenuItemParaDiagramasAbertos := Janelas1;

  // se tiver um primeiro parâmetro, tenta abri-lo como um arquivo de diagrama
  if (ParamStr(1) <> '') and IsDigsERFile(ParamStr(1)) then
    FDiagramaManager.OpenModelo(ParamStr(1))
  else
    // carrega o aplicativo com um modelo em branco para ser usado
    FDiagramaManager.NovoModelo;
    
  FDiagramaManager.OnMudancaEstadoModelo := HabilitarMenusContexto;
  // chama a primeira vez manualmente
  HabilitarMenusContexto(Sender);

  MemoLog := Memo1;

  // ganhar tempo em debug
  FDiagramaManager.OpenModelo('C:\Users\rceleoterio\Documents\sistemas hc.dger');
  //FDiagramaManager.OpenEntityContainer('{EF6BD092-6957-4324-A7F5-09C510A8519B}');
end;

procedure TFormPrincipal.HabilitarMenusContexto(Sender: TObject);
begin
  // define os menus que devem estar habilitados
  Fechar1.Enabled := FDiagramaManager.TemModeloParaSalvar;
  SalvarModelo1.Enabled := FDiagramaManager.TemModeloParaSalvar;
  AdicionarEntidade1.Enabled := FDiagramaManager.EntityContainerCorrente <> nil;
  Abrirdiagrama1.Enabled := FDiagramaManager.TemModeloParaSalvar;
  Remover1.Enabled := FDiagramaManager.EntityContainerCorrente <> nil;
  NovoDiagrama1.Enabled := FDiagramaManager.TemModeloParaSalvar;
  Renomear1.Enabled := FDiagramaManager.EntityContainerCorrente <> nil;
  ExportarparaImagem1.Enabled := FDiagramaManager.EntityContainerCorrente <> nil;
  MoverObjetos1.Enabled := FDiagramaManager.EntityContainerCorrente <> nil;
  
  //define se a janela de adicionar entidades desta estar habilidata
  //pnEntidades.Visible := FDiagramaManager.EntityContainerCorrente <> nil;

  // define o caption da janela principal
  Caption := 'DigsER - Visualizador de Modelo de Dados';
  if FDiagramaManager.TemModeloParaSalvar then
  begin
    if FDiagramaManager.NomeModeloAberto <> '' then
      Caption := Caption + ' [' + FDiagramaManager.NomeModeloAberto + ']'
    else
      Caption := Caption + ' [Modelo sem nome]';
  end;

  // define o nome do diagrama aberto
  if FDiagramaManager.EntityContainerCorrente <> nil then
    StatusBar1.Panels[0].Text := 'Diagrama: ' + FDiagramaManager.EntityContainerCorrente.Titulo
  else
    StatusBar1.Panels[0].Text := 'Nenhum diagrama aberto';
end;

procedure TFormPrincipal.MoverObjetos1Click(Sender: TObject);
begin
  TFormMoverObjetos.MoverObjetos;
end;

procedure TFormPrincipal.Novo1Click(Sender: TObject);
begin
  if FDiagramaManager.TemModeloParaSalvar then
  begin
    {if  Application.MessageBox('Deseja salvar as alterações do Modelo atual?', 'ATENÇÃO',
      MB_YESNO + MB_ICONQUESTION) = IDYES then
    begin
      if FDiagramaManager.SaveModelo then
      begin
        FDiagramaManager.FecharModelo;
        FDiagramaManager.NovoModelo;
      end;
    end
    else
    begin     }
      FDiagramaManager.FecharModelo;
      FDiagramaManager.NovoModelo;
    //end;
  end
  else
    FDiagramaManager.NovoModelo;
end;

procedure TFormPrincipal.NovoDiagrama1Click(Sender: TObject);
begin
  FDiagramaManager.NovoDiagrama;
end;

procedure TFormPrincipal.PesquisarObjetos1Click(Sender: TObject);
begin
  TFormPesquisarTabelas.PesquisarObjetos(False);
end;

procedure TFormPrincipal.Remover1Click(Sender: TObject);
begin
  if Application.MessageBox('Deseja mesmo excluir o Diagrama selecionado?',
                            'ATENÇÃO', MB_YESNO + MB_ICONQUESTION) = IDYES then
    FDiagramaManager.RemoverDiagrama(FDiagramaManager.EntityContainerCorrente.DiagramaId);

end;

procedure TFormPrincipal.Renomear1Click(Sender: TObject);
begin
  FDiagramaManager.RenomearDiagrama(FDiagramaManager.EntityContainerCorrente.DiagramaId);
end;

procedure TFormPrincipal.Sair1Click(Sender: TObject);
begin
  Close;
end;

procedure TFormPrincipal.SalvarModelo1Click(Sender: TObject);
begin
  // o menu salvar só fica ativo se tiver modelo aberto para ser salvo
  FDiagramaManager.SaveModelo;
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
