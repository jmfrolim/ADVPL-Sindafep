/*
+----------------------------------------------------------------------------+
!                         FICHA TECNICA DO PROGRAMA                          !
+----------------------------------------------------------------------------+
!   DADOS DO PROGRAMA                                                        !
+------------------+---------------------------------------------------------+
!Tipo              ! Relatorio                                               !
+------------------+---------------------------------------------------------+
!Modulo            ! FAT                                                     !
+------------------+---------------------------------------------------------+
!Nome              ! RFAT002                                                 !
+------------------+---------------------------------------------------------+
!Descricao         ! Relatório de autorização de débito do Banco CEF.        !
!                  !                                                         !
+------------------+---------------------------------------------------------+
!Autor             ! Marcos Vinicius Perusselo                               !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 22/12/2014                                              !
+------------------+---------------------------------------------------------+
!   ATUALIZACOES                                                             !
+-------------------------------------------+-----------+-----------+--------+
!   Descricao detalhada da atualizacao      !Nome do    ! Analista  !Data da !
!                                           !Solicitante! Respons.  !Atualiz.!
+-------------------------------------------+-----------+-----------+--------+
!                                           !           !           !        !
!                                           !           !           !        !
+-------------------------------------------+-----------+-----------+--------+
*/

#INCLUDE "PROTHEUS.CH"

#DEFINE CRLF CHR(13) + CHR(10)

User Function RFAT002()
	Local oPrint := Nil
	Local oFont10
	Local oFont14
	Local oFont18
	Local oFont24
	Local nRow := 0
	Local cNomeCli := ""
	Local cCPFCli := ""
	Local cAgencia := ""
	Local cNomeAg := ""
	Local aConta := {}
	Local cContaNum := ""
	Local cDV := ""
	Local cOpera := ""
	Local cData := ""
	Local cNomeConv := ""
	Local cCodCompr := ""
	Local cIdentCli := ""
	Local cValMax := ""
	Local cLogo :='\system\caixa.bmp'
	
	oFont10  := TFont():New("Arial",9,10,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont14  := TFont():New("Arial",9,14,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont16  := TFont():New("Arial",9,16,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont18  := TFont():New("Arial",9,18,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont24  := TFont():New("Arial",9,24,.T.,.T.,5,.T.,5,.T.,.F.)
	
	If MsgYesNo("Imprimir dados do cadastro?")
		lDados := .T.
	EndIf	
	
	oPrint:= TMSPrinter():New("Autorizacao Debito CEF")
	oPrint:Setup()
	oPrint:SetPortrait()
	oPrint:StartPage()
	
	If lDados // Se imprimir dados
		If(SA1->A1_XBCOUSR == "N")
			cNomeCli := AllTrim(SA1->A1_XNOME)
			cCPFCli := AllTrim(SA1->A1_XCTACPF)
		Else
			cNomeCli := AllTrim(SA1->A1_NOME)
			cCPFCli := AllTrim(SA1->A1_CGC)
		EndIf

		cIdentCli := cCPFCli + "S"
		
		cAgencia := AllTrim(SA1->A1_XAGENCI)
		// cNomeAg := AllTrim(SA1->A1_XAGENCI)
		
		aConta := StrTokArr(SA1->A1_XCONTA, '-')
		cContaNum := AllTrim(aConta[1])
		If Len(aConta) > 1
			cDV := AllTrim(aConta[2])
		EndIf
		
		cOpera := AllTrim(SA1->A1_XCONTOP)
		
	EndIf	
	cData := DtoC(dDataBase)
	cNomeConv := "SINDAFEP PS"
	cCodCompr := "010887-11-0002"
	cValMax := ""
	
	nRow := 20
	
	oPrint:Say(nRow+10,800,"Autorização de Débito - SIACC",oFont16)
	
	If (File(cLogo))
		oPrint:SayBitmap(nRow+30,90,cLogo,350,80)
	EndIf
	
	oPrint:Say(nRow+200,1600,"Data",oFont10)
	oPrint:Say(nRow+300,1605,cData,oFont14)
	
	oPrint:Line (nRow+250,1600,nRow+350,1600)
	oPrint:Line (nRow+350,1600,nRow+350,2000)
	oPrint:Line (nRow+250,2000,nRow+350,2000)
	
	oPrint:Say(nRow+400,100,"Agência",oFont10)
	oPrint:Say(nRow+500,105,cAgencia,oFont14)
	oPrint:Say(nRow+400,350,"Nome Agência",oFont10)
	oPrint:Say(nRow+500,355,cNomeAg,oFont14)
	oPrint:Say(nRow+400,1100,"Operação",oFont10)
	oPrint:Say(nRow+500,1105,cOpera,oFont14)
	oPrint:Say(nRow+400,1300,"Número da Conta",oFont10)
	oPrint:Say(nRow+500,1305,cContaNum,oFont14)
	oPrint:Say(nRow+400,1650,"DV",oFont10)
	oPrint:Say(nRow+500,1655,cDV,oFont14)
	
	oPrint:Line (nRow+450,0100,nRow+550,0100)
	oPrint:Line (nRow+550,0100,nRow+550,2000)
	oPrint:Line (nRow+450,0350,nRow+550,0350)
	oPrint:Line (nRow+450,1100,nRow+550,1100)
	oPrint:Line (nRow+450,1300,nRow+550,1300)
	oPrint:Line (nRow+450,1650,nRow+550,1650)
	oPrint:Line (nRow+450,2000,nRow+550,2000)
	
	oPrint:Say(nRow+600,0100,"Nome do Cliente",oFont10)
	oPrint:Say(nRow+700,0105,cNomeCli,oFont14)
	
	oPrint:Say(nRow+600,1500,"CPF/CNPJ",oFont10)
	oPrint:Say(nRow+700,1505,cCPFCli,oFont14)
	
	oPrint:Line (nRow+0650,0100,nRow+0750,0100)
	oPrint:Line (nRow+0750,0100,nRow+0750,2000)
	oPrint:Line (nRow+0650,1500,nRow+0750,1500)
	oPrint:Line (nRow+0650,2000,nRow+0750,2000)
	
	oPrint:Say(nRow+800,0100,"Nome do Convenente",oFont10)
	oPrint:Say(nRow+900,0105,cNomeConv,oFont14)
	
	oPrint:Say(nRow+800,0500,"Código do Compromisso",oFont10)
	oPrint:Say(nRow+900,0505,cCodCompr,oFont14)
	
	oPrint:Say(nRow+800,1000,"Identificação do Cliente",oFont10)
	oPrint:Say(nRow+900,1005,cIdentCli,oFont14)
	
	oPrint:Say(nRow+800,1500,"Valor Máximo para Débito",oFont10)
	oPrint:Say(nRow+900,1505,cValMax,oFont14)
	
	oPrint:Line (nRow+0850,0100,nRow+0950,0100)
	oPrint:Line (nRow+0950,0100,nRow+0950,2000)
	oPrint:Line (nRow+0850,2000,nRow+0950,2000)
	oPrint:Line (nRow+0850,0500,nRow+0950,0500)
	oPrint:Line (nRow+0850,1000,nRow+0950,1000)
	oPrint:Line (nRow+0850,1500,nRow+0950,1500)
	
	oPrint:Line (nRow+0950,0100,nRow+1050,0100)
	oPrint:Line (nRow+1050,0100,nRow+1150,0100)
	oPrint:Line (nRow+1150,0100,nRow+1250,0100)
	oPrint:Line (nRow+1250,0100,nRow+1350,0100)
	
	oPrint:Line (nRow+0950,0500,nRow+1050,0500)
	oPrint:Line (nRow+1050,0500,nRow+1150,0500)
	oPrint:Line (nRow+1150,0500,nRow+1250,0500)
	oPrint:Line (nRow+1250,0500,nRow+1350,0500)
	
	oPrint:Line (nRow+0950,1000,nRow+1050,1000)
	oPrint:Line (nRow+1050,1000,nRow+1150,1000)
	oPrint:Line (nRow+1150,1000,nRow+1250,1000)
	oPrint:Line (nRow+1250,1000,nRow+1350,1000)
	
	oPrint:Line (nRow+0950,1500,nRow+1050,1500)
	oPrint:Line (nRow+1050,1500,nRow+1150,1500)
	oPrint:Line (nRow+1150,1500,nRow+1250,1500)
	oPrint:Line (nRow+1250,1500,nRow+1350,1500)
	
	oPrint:Line (nRow+0950,2000,nRow+1050,2000)
	oPrint:Line (nRow+1050,2000,nRow+1150,2000)
	oPrint:Line (nRow+1150,2000,nRow+1250,2000)
	oPrint:Line (nRow+1250,2000,nRow+1350,2000)
	
	oPrint:Line (nRow+1050,0100,nRow+1050,2000)
	oPrint:Line (nRow+1150,0100,nRow+1150,2000)
	oPrint:Line (nRow+1250,0100,nRow+1250,2000)
	oPrint:Line (nRow+1350,0100,nRow+1350,2000)
	
	oPrint:Say(nRow+1450,0100,"Importante:",oFont14)
	oPrint:Say(nRow+1500,0100,"(1) O débito será efetuado de acordo com o valor estipulado acima como valor máximo,",oFont14)
	oPrint:Say(nRow+1550,0100,"quando for o caso, e somente se houver saldo disponível suficiente;",oFont14)
	oPrint:Say(nRow+1600,0100,"(2) No caso de valores superiores ao máximo estipulado, o débito será rejeitado, e neste",oFont14)
	oPrint:Say(nRow+1650,0100,"caso, é de responsabilidade do cliente o pagamento do valor;",oFont14)
	oPrint:Say(nRow+1700,0100,"(3) Os débitos autorizados sem movimentação por mais de 180 dias, serão automaticamente",oFont14)
	oPrint:Say(nRow+1750,0100,"cancelados.",oFont14)
	
	oPrint:Say(nRow+2050,0100,Replicate("_", 40),oFont14)
	oPrint:Say(nRow+2100,0100,"Assinatura do(s) titular(es) da conta",oFont14)
	
	oPrint:Say(nRow+2050,1100,Replicate("_", 40),oFont14)
	oPrint:Say(nRow+2100,1100,"Assinatura e carimbo do empregado responsável pela conferência",oFont14)
	
	oPrint:EndPage()
	oPrint:Preview()
Return
