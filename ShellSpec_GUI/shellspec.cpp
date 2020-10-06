#include "shellspec.h"
#include "ui_shellspec.h"
#include "spectra.h"
#include "setopacities.h"
#include "bodyfrozen.h"
#include "lineofsight.h"
#include "plotting_ss.h"
#include "about_version.h"
#include "proxy_horizontal_tabwidget.h"
#include "include_funcs.h"
#include <QtDebug>
#include <QFileInfo>
#include <QDir>
#include <QMessageBox>
#include <QFileDialog>
#include <QDesktopServices>

void rename_all_output_files();
void rename_one_file(QString, QString);
int run_shellspec();

shellspec::shellspec(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::shellspec)
{
    ui->setupUi(this);
    ui->tabObjects->tabBar()->setStyle(new CustomTabStyle);

    // Create table if not exists
    qDebug()<<"Initial configuration ...";
    if (create_table_only_once()){
        qDebug()<<"Initial setup is OK.";
    }
    QString field = "user";
    set_defaults_main(field);
}

shellspec::~shellspec()
{
    delete ui;
}

void shellspec::on_actionInput_Spectra_triggered()
{
    spectra spectra;
    spectra.setModal(true);
    spectra.exec();
}

void shellspec::on_actionOpacity_triggered()
{
    setOpacities setOpacities;
    setOpacities.setModal(true);
    setOpacities.exec();
}

void shellspec::on_actionBody_Frozen_triggered()
{
    bodyfrozen bodyfrozen;
    bodyfrozen.setModal(true);
    bodyfrozen.exec();
}

void shellspec::on_actionLine_of_Sight_triggered(){
    lineOfSight lineofsight;
    lineofsight.setModal(true);
    lineofsight.exec();
}

void shellspec::on_actionPlot_triggered(){
    plotting_ss plotting_ss;
    plotting_ss.setModal(true);
    plotting_ss.exec();
}

void shellspec::on_checkielnd_toggled(bool checked)
{
    if (checked){
        ui->checkAbundSolar->setChecked(false);
        ui->checkAbundSolar->setDisabled(true);
    } else {
        ui->checkAbundSolar->setDisabled(false);
    }
}

void shellspec::on_groupCSmain_toggled(bool arg1)
{
    if (arg1){
        ui->tabObjects->setTabIcon(0, QIcon(":/resources/icons/green_button.xpm"));
    } else{
        ui->tabObjects->setTabIcon(0, QIcon(":/resources/icons/red_button.xpm"));
    }
}

void shellspec::on_groupCompanion_toggled(bool arg1)
{
    if (arg1){
        ui->tabObjects->setTabIcon(1, QIcon(":/resources/icons/green_button.xpm"));
    } else{
        ui->tabObjects->setTabIcon(1, QIcon(":/resources/icons/red_button.xpm"));
    }
}

void shellspec::on_groupEnvelope_toggled(bool arg1)
{
    if (arg1){
        ui->tabObjects->setTabIcon(2, QIcon(":/resources/icons/green_button.xpm"));
    } else{
        ui->tabObjects->setTabIcon(2, QIcon(":/resources/icons/red_button.xpm"));
    }
}

void shellspec::on_groupSpot_toggled(bool arg1)
{
    if (arg1){
        ui->tabObjects->setTabIcon(3, QIcon(":/resources/icons/green_button.xpm"));
    } else{
        ui->tabObjects->setTabIcon(3, QIcon(":/resources/icons/red_button.xpm"));
    }
}

void shellspec::on_groupStream_toggled(bool arg1)
{
    if (arg1){
        ui->tabObjects->setTabIcon(4, QIcon(":/resources/icons/green_button.xpm"));
    } else{
        ui->tabObjects->setTabIcon(4, QIcon(":/resources/icons/red_button.xpm"));
    }
}

void shellspec::on_groupRing_toggled(bool arg1)
{
    if (arg1){
        ui->tabObjects->setTabIcon(5, QIcon(":/resources/icons/green_button.xpm"));
    } else{
        ui->tabObjects->setTabIcon(5, QIcon(":/resources/icons/red_button.xpm"));
    }
}

void shellspec::on_groupDisc_toggled(bool arg1)
{
    if (arg1){
        ui->tabObjects->setTabIcon(6, QIcon(":/resources/icons/green_button.xpm"));
    } else{
        ui->tabObjects->setTabIcon(6, QIcon(":/resources/icons/red_button.xpm"));
    }
}

void shellspec::on_groupNebula_toggled(bool arg1)
{
    if (arg1){
        ui->tabObjects->setTabIcon(7, QIcon(":/resources/icons/green_button.xpm"));
    } else{
        ui->tabObjects->setTabIcon(7, QIcon(":/resources/icons/red_button.xpm"));
    }
}

void shellspec::on_groupFlow_toggled(bool arg1)
{
    if (arg1){
        ui->tabObjects->setTabIcon(8, QIcon(":/resources/icons/green_button.xpm"));
    } else{
        ui->tabObjects->setTabIcon(8, QIcon(":/resources/icons/red_button.xpm"));
    }
}

void shellspec::on_groupJet_toggled(bool arg1)
{
    if (arg1){
        ui->tabObjects->setTabIcon(9, QIcon(":/resources/icons/green_button.xpm"));
    } else{
        ui->tabObjects->setTabIcon(9, QIcon(":/resources/icons/red_button.xpm"));
    }
}

void shellspec::on_groupUfo_toggled(bool arg1)
{
    if (arg1){
        ui->tabObjects->setTabIcon(10, QIcon(":/resources/icons/green_button.xpm"));
    } else{
        ui->tabObjects->setTabIcon(10, QIcon(":/resources/icons/red_button.xpm"));
    }
}

void shellspec::on_groupShell_toggled(bool arg1)
{
    if (arg1){
        ui->tabObjects->setTabIcon(11, QIcon(":/resources/icons/green_button.xpm"));
    } else{
        ui->tabObjects->setTabIcon(11, QIcon(":/resources/icons/red_button.xpm"));
    }
}

void shellspec::on_actionLoad_in_file_triggered()
{
    include_funcs incfuncs;

    QString directory = QFileDialog::getOpenFileName(this, tr("Load model ..."), QDir::currentPath());
    QFileInfo check_file(directory);
    if (check_file.exists() && check_file.isFile())
    {
        qDebug() << directory << endl;
        incfuncs.load_input_file(directory.toStdString());
        QString field = "user";
        set_defaults_main(field);
    }
    else
    {
        QMessageBox msgBox;
        msgBox.warning(this, "Loading file error.", "No such file or directory:" + directory + ".");
    }
}

void shellspec::on_actionRun_triggered()
{
    include_funcs incfuncs;


    if (save_all_user_inputs()){
        incfuncs.create_input_for_shellspec();
        run_shellspec();
        qDebug()<<"Shellspec Ended correctly.";
    }
    else {
        qDebug()<<"Error ocurred in ShellSpec DB."<<endl;
    }
    rename_all_output_files();
}

int run_shellspec()
{
    shellspec_();
    return 0;
}

void shellspec::on_actionUser_manual_triggered()
{
    QDesktopServices::openUrl(QUrl("https://www.ta3.sk/~budaj/shellspec.html"));
}

bool shellspec::save_all_user_inputs()
{
    const QString SS_DIR = QDir::currentPath()+QDir::separator();
    QString alam1 = ui->textalam1->text();
    QString alamn = ui->textalamn->text();
    QString alams = ui->textalams->text();
    int loglam = 0;
    int ichemc = 0;
    int ielnd = 0;
    int istar = 0;
    int idifst = 0;
    QString rstar = ui->textrstar->text();
    QString tstar = ui->texttsar->text();
    QString emstar = ui->textemstar->text();
    QString xstar = ui->textxstar->text();
    QString ystar = ui->textystar->text();
    QString zstar = ui->textzstar->text();
    QString vrotst = ui->textvrotst->text();
    QString drotst = ui->textdrotst->text();
    QString hst = ui->texthst->text();
    QString vxst = ui->textvxst->text();
    QString vyst = ui->textvyst->text();
    QString vzst = ui->textvzst->text();
    QString dlst = ui->textdlst->text();
    QString dlst2 = ui->textdlst2->text();
    QString dgst = ui->combodgst->currentText();
    QString ffst = ui->textffst->text();
    int irrst = 0;
    int ialbst = 0;
    QString albst = ui->textalbst->text();
    QString htst = ui->texthtst->text();
    QString htsta = ui->texthtsta->text();
    int ispst = 0;
    QString xspst = ui->textxspst->text();
    QString yspst = ui->textyspst->text();
    QString zspst = ui->textzspst->text();
    QString aspst = ui->textaspst->text();
    QString tspst = ui->texttspst->text();
    int icomp = 1;
    QString rcp = ui->textrcp->text();
    QString tempcp = ui->texttempcp->text();
    QString qq = ui->textqq->text();
    QString vrxcp = ui->textvrxcp->text();
    QString vrycp = ui->textvrycp->text();
    QString vrzcp = ui->textvrzcp->text();
    QString vrotcp = ui->textvrotcp->text();
    QString xcp = ui->textxcp->text();
    QString ycp = ui->textycp->text();
    QString zcp = ui->textzcp->text();
    QString vxcp = ui->textvxcp->text();
    QString vycp = ui->textvycp->text();
    QString vzcp = ui->textvzcp->text();
    QString dlcp = ui->textdlcp->text();
    QString dlcp2 = ui->textdlcp2->text();
    QString dgcp = ui->combodgcp->currentText();
    QString ffcp = ui->textffcp->text();
    int irrcp = 0;
    int ialbcp = 0;
    QString albcp = ui->textalbcp->text();
    QString htcp = ui->texthtcp->text();
    QString htcpa = ui->texthtcpa->text();
    int ienv = 1;
    QString emen = ui->textemen->text();
    QString qqen = ui->textqqen->text();
    QString aen = ui->textaen->text();
    QString ffen = ui->textffen->text();
    QString hen = ui->texthen->text();
    QString tempen = ui->texttempen->text();
    QString densen = ui->textdensen->text();
    QString aneen = ui->textaneen->text();
    QString vtrben = ui->textvtrben->text();
    QString dstden = ui->textdstden->text();
    QString dstten = ui->textdstten->text();
    int ispot = 0;
    QString vrxsp = ui->textvrxsp->text();
    QString vrysp = ui->textvrysp->text();
    QString vrzsp = ui->textvrzsp->text();
    QString vrotsp = ui->textvrotsp->text();
    QString rsp = ui->textrsp->text();
    QString xsp = ui->textxsp->text();
    QString ysp = ui->textysp->text();
    QString zsp = ui->textzsp->text();
    QString vxsp = ui->textvxsp->text();
    QString vysp = ui->textvysp->text();
    QString vzsp = ui->textvzsp->text();
    QString tempsp = ui->texttempsp->text();
    QString denssp = ui->textdenssp->text();
    QString anesp = ui->textanesp->text();
    QString vtrbsp = ui->textvtrbsp->text();
    QString dstdsp = ui->textdstdsp->text();
    QString dsttsp = ui->textdsttsp->text();
    int ism = 0;
    QString v1sm = ui->textv1sm->text();
    QString v2sm = ui->textv2sm->text();
    QString r1sm = ui->textr1sm->text();
    QString r2sm = ui->textr2sm->text();
    QString x1sm = ui->textx1sm->text();
    QString y1sm = ui->texty1sm->text();
    QString z1sm = ui->textz1sm->text();
    QString x2sm = ui->textx2sm->text();
    QString y2sm = ui->texty2sm->text();
    QString z2sm = ui->textz2sm->text();
    QString vxsm = ui->textvxsm->text();
    QString vysm = ui->textvysm->text();
    QString vzsm = ui->textvzsm->text();
    QString xsm = ui->textxsm->text();
    QString ysm = ui->textysm->text();
    QString zsm = ui->textzsm->text();
    QString psm = ui->textpsm->text();
    QString tempsm = ui->texttempsm->text();
    QString denssm = ui->textdenssm->text();
    QString anesm = ui->textanesm->text();
    QString vtrbsm = ui->textvtrbsm->text();
    QString edensm = ui->textedensm->text();
    QString dstdsm = ui->textdstdsm->text();
    QString dsttsm = ui->textdsttsm->text();
    int iring = 1;
    QString rrg = ui->textrrg->text();
    QString emrg = ui->textemrg->text();
    QString b1rg = ui->textb1rg->text();
    QString b2rg = ui->textb2rg->text();
    QString a1rg = ui->texta1rg->text();
    QString a2rg = ui->texta2rg->text();
    QString dr1rg = ui->textdr1rg->text();
    QString dr2rg = ui->textdr2rg->text();
    QString xrg = ui->textxrg->text();
    QString yrg = ui->textyrg->text();
    QString zrg = ui->textzrg->text();
    QString xpolrg = ui->textxpolrg->text();
    QString ypolrg = ui->textypolrg->text();
    QString zpolrg = ui->textzpolrg->text();
    QString vxrg = ui->textvxrg->text();
    QString vyrg = ui->textvyrg->text();
    QString vzrg = ui->textvzrg->text();
    QString temprg = ui->texttemprg->text();
    QString densrg = ui->textdensrg->text();
    QString anerg = ui->textanerg->text();
    QString vtrbrg = ui->textvtrbrg->text();
    int itrg = ui->comboitrg->currentIndex() + 1;
    QString edenrg = ui->textedenrg->text();
    QString dstdrg = ui->textdstdrg->text();
    QString ede2rg = ui->textede2rg->text();
    QString dst2rg = ui->textdst2rg->text();
    QString dsttrg = ui->textdsttrg->text();
    int idisc = 0;
    QString adisc = ui->textadisc->text();
    QString rindc = ui->textrindc->text();
    QString routdc = ui->textroutdc->text();
    QString emdc = ui->textemdc->text();
    QString rdc = ui->textrdc->text();
    QString xdc = ui->textxdc->text();
    QString ydc = ui->textydc->text();
    QString zdc = ui->textzdc->text();
    QString xdisc = ui->textxdisc->text();
    QString ydisc = ui->textydisc->text();
    QString zdisc = ui->textzdisc->text();
    QString vxdc = ui->textvxdc->text();
    QString vydc = ui->textvydc->text();
    QString vzdc = ui->textvzdc->text();
    QString tempdc = ui->texttempdc->text();
    QString densdc = ui->textdensdc->text();
    QString anedc = ui->textanedc->text();
    QString vtrbdc = ui->textvtrbdc->text();
    QString edendc = ui->textedendc->text();
    int itdc = ui->comboitdc->currentIndex() + 1;
    QString etmpdc = ui->textetmpdc->text();
    QString dstddc = ui->textdstddc->text();
    QString dsttdc = ui->textdsttdc->text();
    int inebl = 0;
    QString aneb = ui->textaneb->text();
    QString rinnb = ui->textrinnb->text();
    QString routnb = ui->textroutnb->text();
    QString emnb = ui->textemnb->text();
    QString rnb = ui->textrnb->text();
    QString hinvnb = ui->texthinvnb->text();
    QString tinvnb = ui->texttinvnb->text();
    QString hwindnb = ui->texthwindnb->text();
    int idennb = 0;
    QString xneb = ui->textxneb->text();
    QString yneb = ui->textyneb->text();
    QString zneb = ui->textzneb->text();
    QString vxnb = ui->textvxnb->text();
    QString vynb = ui->textvynb->text();
    QString vznb = ui->textvznb->text();
    QString tempnb = ui->texttempnb->text();
    QString densnb = ui->textdensnb->text();
    QString anenb = ui->textanenb->text();
    QString vtrbnb = ui->textvtrbnb->text();
    QString edennb = ui->textedennb->text();
    int itnb = ui->comboitnb->currentIndex() + 1;
    QString etmpnb = ui->textetmpnb->text();
    QString dstdnb = ui->textdstdnb->text();
    QString dsttnb = ui->textdsttnb->text();
    int iflow = 0;
    QString v1fw = ui->textv1fw->text();
    QString v2fw = ui->textv2fw->text();
    QString r1fw = ui->textr1fw->text();
    QString r2fw = ui->textr2fw->text();
    QString x1fw = ui->textx1fw->text();
    QString y1fw = ui->texty1fw->text();
    QString z1fw = ui->textz1fw->text();
    QString x2fw = ui->textx2fw->text();
    QString y2fw = ui->texty2fw->text();
    QString z2fw = ui->textz2fw->text();
    QString vxfw = ui->textvxfw->text();
    QString vyfw = ui->textvyfw->text();
    QString vzfw = ui->textvzfw->text();
    QString xfw = ui->textxfw->text();
    QString yfw = ui->textyfw->text();
    QString zfw = ui->textzfw->text();
    QString pfw = ui->textpfw->text();
    QString tempfw = ui->texttempfw->text();
    QString densfw = ui->textdensfw->text();
    QString anefw = ui->textanefw->text();
    QString vtrbfw = ui->textvtrbfw->text();
    QString edenfw = ui->textedenfw->text();
    QString dstdfw = ui->textdstdfw->text();
    QString dsttfw = ui->textdsttfw->text();
    int ijet = 0;
    QString ajet = ui->textajet->text();
    QString rinjt = ui->textrinjt->text();
    QString routjt = ui->textroutjt->text();
    QString vjt = ui->textvjt->text();
    QString xjet = ui->textxjet->text();
    QString yjet = ui->textyjet->text();
    QString zjet = ui->textzjet->text();
    QString vxjt = ui->textvxjt->text();
    QString vyjt = ui->textvyjt->text();
    QString vzjt = ui->textvzjt->text();
    QString tempjt = ui->texttempjt->text();
    QString densjt = ui->textdensjt->text();
    QString anejt = ui->textanejt->text();
    QString vtrbjt = ui->textvtrbjt->text();
    QString dstdjt = ui->textdstdjt->text();
    QString dsttjt = ui->textdsttjt->text();
    int iufo = 0;
    QString aufo = ui->textaufo->text();
    QString rinuf = ui->textrinuf->text();
    QString routuf = ui->textroutuf->text();
    QString emuf = ui->textemuf->text();
    QString ruf = ui->textruf->text();
    QString xuf = ui->textxuf->text();
    QString yuf = ui->textyuf->text();
    QString zuf = ui->textzuf->text();
    QString xufo = ui->textxufo->text();
    QString yufo = ui->textyufo->text();
    QString zufo = ui->textzufo->text();
    QString vxuf = ui->textvxuf->text();
    QString vyuf = ui->textvyuf->text();
    QString vzuf = ui->textvzuf->text();
    QString tempuf = ui->texttempuf->text();
    QString densuf = ui->textdensuf->text();
    QString aneuf = ui->textaneuf->text();
    QString vtrbuf = ui->textvtrbuf->text();
    QString edenuf = ui->textedenuf->text();
    int ituf = ui->comboituf->currentIndex() + 1;
    QString etmpuf = ui->textetmpuf->text();
    QString dstduf = ui->textdstduf->text();
    QString dsttuf = ui->textdsttuf->text();
    int ishell = 0;
    QString rinsh = ui->textrinsh->text();
    QString routsh = ui->textroutsh->text();
    QString vsh = ui->textvsh->text();
    QString evelsh = ui->textevelsh->text();
    QString rcsh = ui->textrcsh->text();
    QString vxsh = ui->textvxsh->text();
    QString vysh = ui->textvysh->text();
    QString vzsh = ui->textvzsh->text();
    QString tempsh = ui->texttempsh->text();
    QString denssh = ui->textdenssh->text();
    QString anesh = ui->textanesh->text();
    QString vtrbsh = ui->textvtrbsh->text();
    QString dstdsh = ui->textdstdsh->text();
    QString dsttsh = ui->textdsttsh->text();
    QString abund_txt = "";

    // Init Config group
    if (ui->checkloglam->isChecked()){
        loglam = 1;
    } else {
        loglam = 0;
    }
    if (ui->checkAbundSolar->isChecked()){
        ichemc = 0;
    } else {
        ichemc = 1;
    }
    if (ui->checkielnd->isChecked()){
        ielnd = 1;
    } else {
        ielnd = 0;
    }

    if (ichemc == 1){
        abund_txt = ui->textAbundances->text();
        QFileInfo check_file(abund_txt);
        if (check_file.exists() && check_file.isFile()){
            QFile::copy(abund_txt, SS_DIR+"abundances");

        } else {
            ichemc = 0;
            ui->checkAbundSolar->setChecked(true);
            // QFile::copy(SS_DIR+"abundances.default", SS_DIR+"abundances");
            QMessageBox msgBox;
            msgBox.warning(this, "Abundances input file warning.", "No such file or directory:" + abund_txt + ".");
        }
   }

    // CS tab, CS type group
    if (ui->groupCSmain->isChecked()){
        if (ui->radioRotSphere->isChecked()){
            istar = 1;
        }
        else if (ui->radioDetBin->isChecked()) {
            istar = 2;
        } else if (ui->radioContSys->isChecked()) {
            istar = 3;
        }
    } else {
        istar = 0;
    }

    // CS tab, Diff group
    if (ui->groupCSDiffRot->isChecked()){
        if (ui->radioCSDiffRotSmooth->isChecked()){
            idifst = 1;
        } else if (ui->radioCSDiffRotStepFunc->isChecked()){
            idifst = 2;
        }
    } else {
        idifst = 0;
    }

    // CS tab, Irradiation and reflection, albedo group
    if (ui->checkirrst->isChecked()){
        irrst = 1;
    } else {
        irrst = 0;
    }
    if (ui->checkialbst->isChecked()){
        ialbst = 1;
    } else {
        ialbst = 0;
    }

    // CS tab, Spot group
    if (ui->groupispst->isChecked()){
        ispst = 1;
    } else {
        ispst = 0;
    }

    // Comp tab, Comp type group
    if (ui->groupCompanion->isChecked()){
        if (ui->radioRotSpherecp->isChecked()){
            icomp = 1;
        }
        else if (ui->radioDetBincp->isChecked()) {
            icomp = 2;
        }
    } else {
        icomp = 0;
    }

    // Comp tab, Irradiation and reflection, albedo group
    if (ui->checkirrcp->isChecked()){
        irrcp = 1;
    } else {
        irrcp = 0;
    }
    if (ui->checkialbcp->isChecked()){
        ialbcp = 1;
    } else {
        ialbcp = 0;
    }

    // Env tab, Env type group
    if (ui->groupEnvelope->isChecked()){
        if (ui->radioRotSphereen->isChecked()){
            ienv = 1;
        }
        else if (ui->radioDetBinen->isChecked()) {
            ienv = 2;
        }
        else if (ui->radioContSysen->isChecked()) {
            ienv = 3;
        }
    } else {
        ienv = 0;
    }

    // Spot tab, Spot group
    if (ui->groupSpot->isChecked()){
        ispot = 1;
    } else {
        ispot = 0;
    }

    // ISM tab, ISM group
    if (ui->groupStream->isChecked()){
        ism = 1;
    } else {
        ism = 0;
    }

    // RING tab, RING group
    if (ui->groupRing->isChecked()){
        iring = 1;
    } else {
        iring = 0;
    }

    // DISC tab, DISC group
    if (ui->groupDisc->isChecked()){
        if (ui->radioRotwedgedisc->isChecked()){
            idisc = 1;
        }
        else if (ui->radioSlandisc->isChecked()){
            idisc = 2;
        }
        else if (ui->radioRotellipdisc->isChecked()) {
            idisc = 3;
        }
    } else {
        idisc = 0;
    }

    // NEBULA tab, NEBULA group
    if (ui->groupNebula->isChecked()){
        inebl = 4;
    } else {
        inebl = 0;
    }

    // NEBULA tab, NEBULA group
    if (ui->groupFlow->isChecked()){
        iflow = 1;
    } else {
        iflow = 0;
    }

    // JET tab, JET group type
    if (ui->groupJet->isChecked()){
        if (ui->radioOneCone->isChecked()){
            ijet = 1;
        } else if (ui->radioTwoCone->isChecked()){
            ijet = 2;
        }
    } else {
        ijet= 0;
    }

    // UFO tab, UFO group
    if (ui->groupUfo->isChecked()){
        if (ui->radioRotwedgeufo->isChecked()){
            iufo = 1;
        }
        else if (ui->radioSlanufo->isChecked()){
            iufo = 2;
        }
        else if (ui->radioRotellipufo->isChecked()) {
            iufo = 3;
        }
    } else {
        iufo = 0;
    }

    // SHELL tab, SHELL group
    if (ui->groupShell->isChecked()){
        if (ui->radioRotwedgesh->isChecked()){
            ishell = 1;
        }
        else if (ui->radioSlansh->isChecked()){
            ishell = 2;
        }
        else if (ui->radioRotellipsh->isChecked()) {
            ishell = 3;
        }
    } else {
        ishell = 0;
    }

    // Saving inputs in the DB
    connOpen();
    QSqlQuery qry;
    qry.prepare("UPDATE user_inputs SET alam1=:alam1, alamn=:alamn, alams=:alams, loglam=:loglam,"
                "ISTAR=:istar, rstar=:rstar, Tstar=:tstar, eMstar=:emstar,"
                " ichemc=:ichemc, ielnd=:ielnd,"
                " xstar=:xstar, ystar=:ystar, zstar=:zstar, vrotst=:vrotst, idifst=:idifst,"
                " drotst=:drotst, hst=:hst, vxst=:vxst, vyst=:vyst, vzst=:vzst, dlst=:dlst,"
                " dlst2=:dlst2, dgst=:dgst, ffst=:ffst, irrst=:irrst, ialbst=:ialbst, albst=:albst,"
                " htst=:htst, htsta=:htsta, ispst=:ispst, xspst=:xspst, yspst=:yspst,"
                " zspst=:zspst, aspst=:aspst, tspst=:tspst,"
                "ICOMP=:icomp, rcp=:rcp, tempcp=:tempcp,"
                " qq=:qq, vrxcp=:vrxcp, vrycp=:vrycp, vrzcp=:vrzcp, vrotcp=:vrotcp, xcp=:xcp,"
                " ycp=:ycp, zcp=:zcp, vxcp=:vxcp, vycp=:vycp, vzcp=:vzcp, dlcp=:dlcp, dlcp2=:dlcp2,"
                " dgcp=:dgcp, ffcp=:ffcp, irrcp=:irrcp, ialbcp=:ialbcp, albcp=:albcp, htcp=:htcp,"
                " htcpa=:htcpa,"
                " IENV=:ienv, emen=:emen, qqen=:qqen, aen=:aen, ffen=:ffen,"
                " hen=:hen, tempen=:tempen, densen=:densen, aneen=:aneen, vtrben=:vtrben,"
                " dstden=:dstden, dstten=:dstten,"
                "ISPOT=:ispot, vrxsp=:vrxsp, vrysp=:vrysp,"
                " vrzsp=:vrzsp, vrotsp=:vrotsp, rsp=:rsp, xsp=:xsp, ysp=:ysp, zsp=:zsp,"
                " vxsp=:vxsp, vysp=:vysp, vzsp=:vzsp, tempsp=:tempsp, denssp=:denssp,"
                " anesp=:anesp, vtrbsp=:vtrbsp, dstdsp=:dstdsp, dsttsp=:dsttsp,"
                " ISM=:ism, v1sm=:v1sm, v2sm=:v2sm, r1sm=:r1sm, r2sm=:r2sm, x1sm=:x1sm,"
                " y1sm=:y1sm, z1sm=:z1sm, x2sm=:x2sm, y2sm=:y2sm, z2sm=:z2sm, vxsm=:vxsm,"
                " vysm=:vysm, vzsm=:vzsm, xsm=:xsm, ysm=:ysm, zsm=:zsm, psm=:psm,"
                " tempsm=:tempsm, denssm=:denssm,anesm=:anesm, vtrbsm=:vtrbsm,"
                " edensm=:edensm, dstdsm=:dstdsm, dsttsm=:dsttsm,"
                "IRING=:iring, rrg=:rrg, emrg=:emrg, b1rg=:b1rg, b2rg=:b2rg, a1rg=:a1rg, a2rg=:a2rg,"
                " dr1rg=:dr1rg, dr2rg=:dr2rg, xrg=:xrg, yrg=:yrg, zrg=:zrg, xpolrg=:xpolrg, ypolrg=:ypolrg,"
                " zpolrg=:zpolrg, vxrg=:vxrg, vyrg=:vyrg, vzrg=:vzrg, temprg=:temprg, densrg=:densrg,"
                " anerg=:anerg, vtrbrg=:vtrbrg, itrg=:itrg, edenrg=:edenrg, dstdrg=:dstdrg,"
                " ede2rg=:ede2rg, dst2rg=:dst2rg, dsttrg=:dsttrg,"
                "IDISC=:idisc, adisc=:adisc, rindc=:rindc, routdc=:routdc, emdc=:emdc,"
                " rdc=:rdc, xdc=:xdc, ydc=:ydc, zdc=:zdc, xdisc=:xdisc, ydisc=:ydisc,"
                " zdisc=:zdisc, vxdc=:vxdc, vydc=:vydc, vzdc=:vzdc, tempdc=:tempdc,"
                " densdc=:densdc, anedc=:anedc, vtrbdc=:vtrbdc, edendc=:edendc, itdc=:itdc, dstddc=:dstddc,"
                " dsttdc=:dsttdc,"
                "INEBL=:inebl, aneb=:aneb, rinnb=:rinnb, routnb=:routnb, emnb=:emnb, rnb=:rnb,"
                " hinvnb=:hinvnb, tinvnb=:tinvnb, hwindnb=:hwindnb, idennb=:idennb, xneb=:xneb,"
                " yneb=:yneb, zneb=:zneb, vxnb=:vxnb, vynb=:vynb,"
                " vznb=:vznb, tempnb=:tempnb, densnb=:densnb, anenb=:anenb, vtrbnb=:vtrbnb,"
                " edennb=:edennb, itnb=:itnb, etmpnb=:etmpnb, dstdnb=:dstdnb, dsttnb=:dsttnb,"
                "IFLOW=:iflow, v1fw=:v1fw, v2fw=:v2fw, r1fw=:r1fw, r2fw=:r2fw, x1fw=:x1fw,"
                " y1fw=:y1fw, z1fw=:z1fw, x2fw=:x2fw, y2fw=:y2fw, z2fw=:z2fw, vxfw=:vxfw,"
                " vyfw=:vyfw, vzfw=:vzfw, xfw=:xfw, yfw=:yfw, zfw=:zfw, pfw=:pfw, tempfw=:tempfw,"
                " densfw=:densfw, anefw=:anefw, vtrbfw=:vtrbfw, edenfw=:edenfw, dstdfw=:dstdfw,"
                " dsttfw=:dsttfw,"
                "IJET=:ijet, ajet=:ajet, rinjt=:rinjt, routjt=:routjt, vjt=:vjt, xjet=:xjet,"
                " yjet=:yjet, zjet=:zjet, vxjt=:vxjt, vyjt=:vyjt, vzjt=:vzjt, tempjt=:tempjt,"
                " densjt=:densjt, anejt=:anejt, vtrbjt=:vtrbjt, dstdjt=:dstdjt, dsttjt=:dsttjt,"
                "IUFO=:iufo, aufo=:aufo, rinuf=:rinuf, routuf=:routuf, emuf=:emuf, ruf=:ruf,"
                " xuf=:xuf, yuf=:yuf, zuf=:zuf, xufo=:xufo, yufo=:yufo, zufo=:zufo,"
                " vxuf=:vxuf, vyuf=:vyuf, vzuf=:vzuf, tempuf=:tempuf, densuf=:densuf,"
                " aneuf=:aneuf, vtrbuf=:vtrbuf, edenuf=:edenuf, ituf=:ituf, etmpuf=:etmpuf,"
                " dstduf=:dstduf, dsttuf=:dsttuf,"
                "ISHELL=:ishell, rinsh=:rinsh, routsh=:routsh, vsh=:vsh, evelsh=:evelsh,"
                " rcsh=:rcsh, vxsh=:vxsh, vysh=:vysh, vzsh=:vzsh, tempsh=:tempsh, denssh=:denssh,"
                " anesh=:anesh, vtrbsh=:vtrbsh, dstdsh=:dstdsh, dsttsh=:dsttsh,"
                " abund_txt=:abund_txt, prefix=:prefix"
                " WHERE ID=2");
    qry.bindValue(":alam1", alam1);
    qry.bindValue(":alamn", alamn);
    qry.bindValue(":alams", alams);
    qry.bindValue(":loglam", loglam);
    qry.bindValue(":ichemc", ichemc);
    qry.bindValue(":ielnd", ielnd);
    qry.bindValue(":istar", istar);
    qry.bindValue(":rstar", rstar);
    qry.bindValue(":tstar", tstar);
    qry.bindValue(":emstar", emstar);
    qry.bindValue(":xstar", xstar);
    qry.bindValue(":ystar", ystar);
    qry.bindValue(":zstar", zstar);
    qry.bindValue(":vrotst", vrotst);
    qry.bindValue(":idifst", idifst);
    qry.bindValue(":drotst", drotst);
    qry.bindValue(":hst", hst);
    qry.bindValue(":vxst", vxst);
    qry.bindValue(":vyst", vyst);
    qry.bindValue(":vzst", vzst);
    qry.bindValue(":dlst", dlst);
    qry.bindValue(":dlst2", dlst2);
    qry.bindValue(":dgst", dgst);
    qry.bindValue(":ffst", ffst);
    qry.bindValue(":irrst", irrst);
    qry.bindValue(":ialbst", ialbst);
    qry.bindValue(":albst", albst);
    qry.bindValue(":htst", htst);
    qry.bindValue(":htsta", htsta);
    qry.bindValue(":ispst", ispst);
    qry.bindValue(":xspst", xspst);
    qry.bindValue(":yspst", yspst);
    qry.bindValue(":zspst", zspst);
    qry.bindValue(":aspst", aspst);
    qry.bindValue(":tspst", tspst);
    qry.bindValue(":icomp", icomp);
    qry.bindValue(":rcp", rcp);
    qry.bindValue(":tempcp", tempcp);
    qry.bindValue(":qq", qq);
    qry.bindValue(":vrxcp", vrxcp);
    qry.bindValue(":vrycp", vrycp);
    qry.bindValue(":vrzcp", vrzcp);
    qry.bindValue(":vrotcp", vrotcp);
    qry.bindValue(":xcp", xcp);
    qry.bindValue(":ycp", ycp);
    qry.bindValue(":zcp", zcp);
    qry.bindValue(":vxcp", vxcp);
    qry.bindValue(":vycp", vycp);
    qry.bindValue(":vzcp", vzcp);
    qry.bindValue(":dlcp", dlcp);
    qry.bindValue(":dlcp2", dlcp2);
    qry.bindValue(":dgcp", dgcp);
    qry.bindValue(":ffcp", ffcp);
    qry.bindValue(":irrcp", irrcp);
    qry.bindValue(":ialbcp", ialbcp);
    qry.bindValue(":albcp", albcp);
    qry.bindValue(":htcp", htcp);
    qry.bindValue(":htcpa", htcpa);
    qry.bindValue(":ienv", ienv);
    qry.bindValue(":emen", emen);
    qry.bindValue(":qqen", qqen);
    qry.bindValue(":aen", aen);
    qry.bindValue(":ffen", ffen);
    qry.bindValue(":hen", hen);
    qry.bindValue(":tempen", tempen);
    qry.bindValue(":densen", densen);
    qry.bindValue(":aneen", aneen);
    qry.bindValue(":vtrben", vtrben);
    qry.bindValue(":dstden", dstden);
    qry.bindValue(":dstten", dstten);
    qry.bindValue(":ispot", ispot);
    qry.bindValue(":vrxsp", vrxsp);
    qry.bindValue(":vrysp", vrysp);
    qry.bindValue(":vrzsp", vrzsp);
    qry.bindValue(":vrotsp", vrotsp);
    qry.bindValue(":rsp", rsp);
    qry.bindValue(":xsp", xsp);
    qry.bindValue(":ysp", ysp);
    qry.bindValue(":zsp", zsp);
    qry.bindValue(":vxsp", vxsp);
    qry.bindValue(":vysp", vysp);
    qry.bindValue(":vzsp", vzsp);
    qry.bindValue(":tempsp", tempsp);
    qry.bindValue(":denssp", denssp);
    qry.bindValue(":anesp", anesp);
    qry.bindValue(":vtrbsp", vtrbsp);
    qry.bindValue(":dstdsp", dstdsp);
    qry.bindValue(":dsttsp", dsttsp);
    qry.bindValue(":ism", ism);
    qry.bindValue(":v1sm", v1sm);
    qry.bindValue(":v2sm", v2sm);
    qry.bindValue(":r1sm", r1sm);
    qry.bindValue(":r2sm", r2sm);
    qry.bindValue(":x1sm", x1sm);
    qry.bindValue(":y1sm", y1sm);
    qry.bindValue(":z1sm", z1sm);
    qry.bindValue(":x2sm", x2sm);
    qry.bindValue(":y2sm", y2sm);
    qry.bindValue(":z2sm", z2sm);
    qry.bindValue(":vxsm", vxsm);
    qry.bindValue(":vysm", vysm);
    qry.bindValue(":vzsm", vzsm);
    qry.bindValue(":xsm", xsm);
    qry.bindValue(":ysm", ysm);
    qry.bindValue(":zsm", zsm);
    qry.bindValue(":psm", psm);
    qry.bindValue(":tempsm", tempsm);
    qry.bindValue(":denssm", denssm);
    qry.bindValue(":anesm", anesm);
    qry.bindValue(":vtrbsm", vtrbsm);
    qry.bindValue(":edensm", edensm);
    qry.bindValue(":dstdsm", dstdsm);
    qry.bindValue(":dsttsm", dsttsm);
    qry.bindValue(":iring", iring);
    qry.bindValue(":rrg", rrg);
    qry.bindValue(":emrg", emrg);
    qry.bindValue(":b1rg", b1rg);
    qry.bindValue(":b2rg", b2rg);
    qry.bindValue(":a1rg", a1rg);
    qry.bindValue(":a2rg", a2rg);
    qry.bindValue(":dr1rg", dr1rg);
    qry.bindValue(":dr2rg", dr2rg);
    qry.bindValue(":xrg", xrg);
    qry.bindValue(":yrg", yrg);
    qry.bindValue(":zrg", zrg);
    qry.bindValue(":xpolrg", xpolrg);
    qry.bindValue(":ypolrg", ypolrg);
    qry.bindValue(":zpolrg", zpolrg);
    qry.bindValue(":vxrg", vxrg);
    qry.bindValue(":vyrg", vyrg);
    qry.bindValue(":vzrg", vzrg);
    qry.bindValue(":temprg", temprg);
    qry.bindValue(":densrg", densrg);
    qry.bindValue(":anerg", anerg);
    qry.bindValue(":vtrbrg", vtrbrg);
    qry.bindValue(":itrg", itrg);
    qry.bindValue(":edenrg", edenrg);
    qry.bindValue(":dstdrg", dstdrg);
    qry.bindValue(":ede2rg", ede2rg);
    qry.bindValue(":dst2rg", dst2rg);
    qry.bindValue(":dsttrg", dsttrg);
    qry.bindValue(":idisc", idisc);
    qry.bindValue(":adisc", adisc);
    qry.bindValue(":rindc", rindc);
    qry.bindValue(":routdc", routdc);
    qry.bindValue(":emdc", emdc);
    qry.bindValue(":rdc", rdc);
    qry.bindValue(":xdc", xdc);
    qry.bindValue(":ydc", ydc);
    qry.bindValue(":zdc", zdc);
    qry.bindValue(":xdisc", xdisc);
    qry.bindValue(":ydisc", ydisc);
    qry.bindValue(":zdisc", zdisc);
    qry.bindValue(":vxdc", vxdc);
    qry.bindValue(":vydc", vydc);
    qry.bindValue(":vzdc", vzdc);
    qry.bindValue(":tempdc", tempdc);
    qry.bindValue(":densdc", densdc);
    qry.bindValue(":anedc", anedc);
    qry.bindValue(":vtrbdc", vtrbdc);
    qry.bindValue(":edendc", edendc);
    qry.bindValue(":itdc", itdc);
    qry.bindValue(":etmpdc", etmpdc);
    qry.bindValue(":dstddc", dstddc);
    qry.bindValue(":dsttdc", dsttdc);
    qry.bindValue(":inebl", inebl);
    qry.bindValue(":aneb", aneb);
    qry.bindValue(":rinnb", rinnb);
    qry.bindValue(":routnb", routnb);
    qry.bindValue(":emnb", emnb);
    qry.bindValue(":rnb", rnb);
    qry.bindValue(":hinvnb", hinvnb);
    qry.bindValue(":tinvnb", tinvnb);
    qry.bindValue(":hwindnb", hwindnb);
    qry.bindValue(":idennb", idennb);
    qry.bindValue(":xneb", xneb);
    qry.bindValue(":yneb", yneb);
    qry.bindValue(":zneb", zneb);
    qry.bindValue(":vxnb", vxnb);
    qry.bindValue(":vynb", vynb);
    qry.bindValue(":vznb", vznb);
    qry.bindValue(":tempnb", tempnb);
    qry.bindValue(":densnb", densnb);
    qry.bindValue(":anenb", anenb);
    qry.bindValue(":vtrbnb", vtrbnb);
    qry.bindValue(":edennb", edennb);
    qry.bindValue(":itnb", itnb);
    qry.bindValue(":etmpnb", etmpnb);
    qry.bindValue(":dstdnb", dstdnb);
    qry.bindValue(":dsttnb", dsttnb);
    qry.bindValue(":iflow", iflow);
    qry.bindValue(":v1fw", v1fw);
    qry.bindValue(":v2fw", v2fw);
    qry.bindValue(":r1fw", r1fw);
    qry.bindValue(":r2fw", r2fw);
    qry.bindValue(":x1fw", x1fw);
    qry.bindValue(":y1fw", y1fw);
    qry.bindValue(":z1fw", z1fw);
    qry.bindValue(":x2fw", x2fw);
    qry.bindValue(":y2fw", y2fw);
    qry.bindValue(":z2fw", z2fw);
    qry.bindValue(":vxfw", vxfw);
    qry.bindValue(":vyfw", vyfw);
    qry.bindValue(":vzfw", vzfw);
    qry.bindValue(":xfw", xfw);
    qry.bindValue(":yfw", yfw);
    qry.bindValue(":zfw", zfw);
    qry.bindValue(":pfw", pfw);
    qry.bindValue(":tempfw", tempfw);
    qry.bindValue(":densfw", densfw);
    qry.bindValue(":anefw", anefw);
    qry.bindValue(":vtrbfw", vtrbfw);
    qry.bindValue(":edenfw", edenfw);
    qry.bindValue(":dstdfw", dstdfw);
    qry.bindValue(":dsttfw", dsttfw);
    qry.bindValue(":ijet", ijet);
    qry.bindValue(":ajet", ajet);
    qry.bindValue(":rinjt", rinjt);
    qry.bindValue(":routjt", routjt);
    qry.bindValue(":vjt", vjt);
    qry.bindValue(":xjet", xjet);
    qry.bindValue(":yjet", yjet);
    qry.bindValue(":zjet", zjet);
    qry.bindValue(":vxjt", vxjt);
    qry.bindValue(":vyjt", vyjt);
    qry.bindValue(":vzjt", vzjt);
    qry.bindValue(":tempjt", tempjt);
    qry.bindValue(":densjt", densjt);
    qry.bindValue(":anejt", anejt);
    qry.bindValue(":vtrbjt", vtrbjt);
    qry.bindValue(":dstdjt", dstdjt);
    qry.bindValue(":dsttjt", dsttjt);
    qry.bindValue(":iufo", iufo);
    qry.bindValue(":aufo", aufo);
    qry.bindValue(":rinuf", rinuf);
    qry.bindValue(":routuf", routuf);
    qry.bindValue(":emuf", emuf);
    qry.bindValue(":ruf", ruf);
    qry.bindValue(":xuf", xuf);
    qry.bindValue(":yuf", yuf);
    qry.bindValue(":zuf", zuf);
    qry.bindValue(":xufo", xufo);
    qry.bindValue(":yufo", yufo);
    qry.bindValue(":zufo", zufo);
    qry.bindValue(":vxuf", vxuf);
    qry.bindValue(":vyuf", vyuf);
    qry.bindValue(":vzuf", vzuf);
    qry.bindValue(":tempuf", tempuf);
    qry.bindValue(":densuf", densuf);
    qry.bindValue(":aneuf", aneuf);
    qry.bindValue(":vtrbuf", vtrbuf);
    qry.bindValue(":edenuf", edenuf);
    qry.bindValue(":ituf", ituf);
    qry.bindValue(":etmpuf", etmpuf);
    qry.bindValue(":dstduf", dstduf);
    qry.bindValue(":dsttuf", dsttuf);
    qry.bindValue(":ishell", ishell);
    qry.bindValue(":rinsh", rinsh);
    qry.bindValue(":routsh", routsh);
    qry.bindValue(":vsh", vsh);
    qry.bindValue(":evelsh", evelsh);
    qry.bindValue(":rcsh", rcsh);
    qry.bindValue(":vxsh", vxsh);
    qry.bindValue(":vysh", vysh);
    qry.bindValue(":vzsh", vzsh);
    qry.bindValue(":tempsh", tempsh);
    qry.bindValue(":denssh", denssh);
    qry.bindValue(":anesh", anesh);
    qry.bindValue(":vtrbsh", vtrbsh);
    qry.bindValue(":dstdsh", dstdsh);
    qry.bindValue(":dsttsh", dsttsh);
    qry.bindValue(":abund_txt", abund_txt);
    qry.bindValue(":prefix", ui->textPrefix->text());


    if (qry.exec())
    {
        qDebug()<<"ShellSpec DB correctly."<<endl;
        connClose();
        return true;
    }
    else {
        qDebug()<<"Error ShellSpec DB."<<qry.lastError()<<endl;
        qDebug()<<qry.lastQuery()<<endl;
        connClose();
        return false;
    }

}

void shellspec::set_defaults_main(QString field)
{
    qDebug() << "Setting default values" << endl;
    QString id = "1";
    if (field == "user"){
        id = "2";
    } else if (field == "default"){
        id = "1";
    }
    connOpen();
    QSqlQueryModel qry;
    qry.setQuery("SELECT * FROM user_inputs WHERE ID="+id);
//    qDebug()<<"Error creating input for shellspec: "<<qry.lastError()<<endl;
    if (qry.record(0).value("ISTAR").toInt() != 0){
        ui->groupCSmain->setChecked(true);
        on_groupCSmain_toggled(true);
    }
    else {
        ui->groupCSmain->setChecked(false);
        on_groupCSmain_toggled(false);
    }
    if (qry.record(0).value("ICOMP").toInt() != 0){
        ui->groupCompanion->setChecked(true);
        on_groupCompanion_toggled(true);
    }
    else {
        ui->groupCompanion->setChecked(false);
        on_groupCompanion_toggled(false);
    }
    if (qry.record(0).value("IENV").toInt() != 0){
        ui->groupEnvelope->setChecked(true);
        on_groupEnvelope_toggled(true);
    }
    else {
        ui->groupEnvelope->setChecked(false);
        on_groupEnvelope_toggled(false);
    }
    if (qry.record(0).value("ISPOT").toInt() != 0){
        ui->groupSpot->setChecked(true);
        on_groupSpot_toggled(true);
    }
    else {
        ui->groupSpot->setChecked(false);
        on_groupSpot_toggled(false);
    }
    if (qry.record(0).value("ISM").toInt() != 0){
        ui->groupStream->setChecked(true);
        on_groupStream_toggled(true);
    }
    else {
        ui->groupStream->setChecked(false);
        on_groupStream_toggled(false);
    }
    if (qry.record(0).value("IRING").toInt() != 0){
        ui->groupRing->setChecked(true);
        on_groupRing_toggled(true);
    }
    else {
        ui->groupRing->setChecked(false);
        on_groupRing_toggled(false);
    }
    if (qry.record(0).value("IDISC").toInt() != 0){
        ui->groupDisc->setChecked(true);
        on_groupDisc_toggled(true);
    }
    else {
        ui->groupDisc->setChecked(false);
        on_groupDisc_toggled(false);
    }
    if (qry.record(0).value("INEBL").toInt() != 0){
        ui->groupNebula->setChecked(true);
        on_groupNebula_toggled(true);
    }
    else {
        ui->groupNebula->setChecked(false);
        on_groupNebula_toggled(false);
    }
    if (qry.record(0).value("IFLOW").toInt() != 0){
        ui->groupFlow->setChecked(true);
        on_groupFlow_toggled(true);
    }
    else {
        ui->groupFlow->setChecked(false);
        on_groupFlow_toggled(false);
    }
    if (qry.record(0).value("IJET").toInt() != 0){
        ui->groupJet->setChecked(true);
        on_groupJet_toggled(true);
    }
    else {
        ui->groupJet->setChecked(false);
        on_groupJet_toggled(false);
    }
    if (qry.record(0).value("IUFO").toInt() != 0){
        ui->groupUfo->setChecked(true);
        on_groupUfo_toggled(true);
    }
    else {
        ui->groupUfo->setChecked(false);
        on_groupUfo_toggled(false);
    }
    if (qry.record(0).value("ISHELL").toInt() != 0){
        ui->groupShell->setChecked(true);
        on_groupShell_toggled(true);
    }
    else {
        ui->groupShell->setChecked(false);
        on_groupShell_toggled(false);
    }

    ui->textalam1->setText(qry.record(0).value("alam1").toString());
    ui->textalamn->setText(qry.record(0).value("alamn").toString());
    ui->textalams->setText(qry.record(0).value("alams").toString());
    ui->textAbundances->setText(qry.record(0).value("abund_txt").toString());
    ui->textrstar->setText(qry.record(0).value("rstar").toString());
    ui->texttsar->setText(qry.record(0).value("Tstar").toString());
    ui->textemstar->setText(qry.record(0).value("eMstar").toString());
    ui->textxstar->setText(qry.record(0).value("xstar").toString());
    ui->textystar->setText(qry.record(0).value("ystar").toString());
    ui->textzstar->setText(qry.record(0).value("zstar").toString());
    ui->textvrotst->setText(qry.record(0).value("vrotst").toString());
//    ui->textidifst->setText(qry.record(0).value("idifst,").toString());
    ui->textdrotst->setText(qry.record(0).value("drotst").toString());
    ui->texthst->setText(qry.record(0).value("hst").toString());
    ui->textvxst->setText(qry.record(0).value("vxst").toString());
    ui->textvyst->setText(qry.record(0).value("vyst").toString());
    ui->textvzst->setText(qry.record(0).value("vzst").toString());
    ui->textdlst->setText(qry.record(0).value("dlst").toString());
    ui->textdlst2->setText(qry.record(0).value("dlst2").toString());
//    ui->textdgst->setText(qry.record(0).value("dgst").toString());
    ui->textffst->setText(qry.record(0).value("ffst").toString());
//    ui->textirrst->setText(qry.record(0).value("irrst").toString());
//    ui->textialbst->setText(qry.record(0).value("ialbst").toString());
    ui->textalbst->setText(qry.record(0).value("albst").toString());
    ui->texthtst->setText(qry.record(0).value("htst").toString());
    ui->texthtsta->setText(qry.record(0).value("htsta").toString());
//    ui->textispst->setText(qry.record(0).value("ispst").toString());
    ui->textxspst->setText(qry.record(0).value("xspst").toString());
    ui->textyspst->setText(qry.record(0).value("yspst").toString());
    ui->textzspst->setText(qry.record(0).value("zspst").toString());
    ui->textaspst->setText(qry.record(0).value("aspst").toString());
    ui->texttspst->setText(qry.record(0).value("tspst").toString());
    ui->textrcp->setText(qry.record(0).value("rcp").toString());
    ui->texttempcp->setText(qry.record(0).value("tempcp").toString());
    ui->textqq->setText(qry.record(0).value("qq").toString());
    ui->textvrxcp->setText(qry.record(0).value("vrxcp").toString());
    ui->textvrycp->setText(qry.record(0).value("vrycp").toString());
    ui->textvrzcp->setText(qry.record(0).value("vrzcp").toString());
    ui->textvrotcp->setText(qry.record(0).value("vrotcp").toString());
    ui->textxcp->setText(qry.record(0).value("xcp").toString());
    ui->textycp->setText(qry.record(0).value("ycp").toString());
    ui->textzcp->setText(qry.record(0).value("zcp").toString());
    ui->textvxcp->setText(qry.record(0).value("vxcp").toString());
    ui->textvycp->setText(qry.record(0).value("vycp").toString());
    ui->textvzcp->setText(qry.record(0).value("vzcp").toString());
    ui->textdlcp->setText(qry.record(0).value("dlcp").toString());
    ui->textdlcp2->setText(qry.record(0).value("dlcp2").toString());
//    ui->textdgcp->setText(qry.record(0).value("dgcp").toString());
    ui->textffcp->setText(qry.record(0).value("ffcp").toString());
//    ui->textirrcp->setText(qry.record(0).value("irrcp").toString());
//    ui->textialbcp->setText(qry.record(0).value("ialbcp").toString());
    ui->textalbcp->setText(qry.record(0).value("albcp").toString());
    ui->texthtcp->setText(qry.record(0).value("htcp").toString());
    ui->texthtcpa->setText(qry.record(0).value("htcpa").toString());
    ui->textemen->setText(qry.record(0).value("emen").toString());
    ui->textqqen->setText(qry.record(0).value("qqen").toString());
    ui->textaen->setText(qry.record(0).value("aen").toString());
    ui->textffen->setText(qry.record(0).value("ffen").toString());
    ui->texthen->setText(qry.record(0).value("hen").toString());
    ui->texttempen->setText(qry.record(0).value("tempen").toString());
    ui->textdensen->setText(qry.record(0).value("densen").toString());
    ui->textaneen->setText(qry.record(0).value("aneen").toString());
    ui->textvtrben->setText(qry.record(0).value("vtrben").toString());
    ui->textdstden->setText(qry.record(0).value("dstden").toString());
    ui->textdstten->setText(qry.record(0).value("dstten").toString());
    ui->textvrxsp->setText(qry.record(0).value("vrxsp").toString());
    ui->textvrysp->setText(qry.record(0).value("vrysp").toString());
    ui->textvrzsp->setText(qry.record(0).value("vrzsp").toString());
    ui->textvrotsp->setText(qry.record(0).value("vrotsp").toString());
    ui->textrsp->setText(qry.record(0).value("rsp").toString());
    ui->textxsp->setText(qry.record(0).value("xsp").toString());
    ui->textysp->setText(qry.record(0).value("ysp").toString());
    ui->textzsp->setText(qry.record(0).value("zsp").toString());
    ui->textvxsp->setText(qry.record(0).value("vxsp").toString());
    ui->textvysp->setText(qry.record(0).value("vysp").toString());
    ui->textvzsp->setText(qry.record(0).value("vzsp").toString());
    ui->texttempsp->setText(qry.record(0).value("tempsp").toString());
    ui->textdenssp->setText(qry.record(0).value("denssp").toString());
    ui->textanesp->setText(qry.record(0).value("anesp").toString());
    ui->textvtrbsp->setText(qry.record(0).value("vtrbsp").toString());
    ui->textdstdsp->setText(qry.record(0).value("dstdsp").toString());
    ui->textdsttsp->setText(qry.record(0).value("dsttsp").toString());
    ui->textv1sm->setText(qry.record(0).value("v1sm").toString());
    ui->textv2sm->setText(qry.record(0).value("v2sm").toString());
    ui->textr1sm->setText(qry.record(0).value("r1sm").toString());
    ui->textr2sm->setText(qry.record(0).value("r2sm").toString());
    ui->textx1sm->setText(qry.record(0).value("x1sm").toString());
    ui->texty1sm->setText(qry.record(0).value("y1sm").toString());
    ui->textz1sm->setText(qry.record(0).value("z1sm").toString());
    ui->textx2sm->setText(qry.record(0).value("x2sm").toString());
    ui->texty2sm->setText(qry.record(0).value("y2sm").toString());
    ui->textz2sm->setText(qry.record(0).value("z2sm").toString());
    ui->textvxsm->setText(qry.record(0).value("vxsm").toString());
    ui->textvysm->setText(qry.record(0).value("vysm").toString());
    ui->textvzsm->setText(qry.record(0).value("vzsm").toString());
    ui->texttempsm->setText(qry.record(0).value("tempsm").toString());
    ui->textdenssm->setText(qry.record(0).value("denssm").toString());
    ui->textanesm->setText(qry.record(0).value("anesm").toString());
    ui->textvtrbsm->setText(qry.record(0).value("vtrbsm").toString());
    ui->textedensm->setText(qry.record(0).value("edensm").toString());
    ui->textdstdsm->setText(qry.record(0).value("dstdsm").toString());
    ui->textdsttsm->setText(qry.record(0).value("dsttsm").toString());
    ui->textrrg->setText(qry.record(0).value("rrg").toString());
    ui->textemrg->setText(qry.record(0).value("emrg").toString());
    ui->textb1rg->setText(qry.record(0).value("b1rg").toString());
    ui->textb2rg->setText(qry.record(0).value("b2rg").toString());
    ui->texta1rg->setText(qry.record(0).value("a1rg").toString());
    ui->texta2rg->setText(qry.record(0).value("a2rg").toString());
    ui->textdr1rg->setText(qry.record(0).value("dr1rg").toString());
    ui->textdr2rg->setText(qry.record(0).value("dr2rg").toString());
    ui->textxrg->setText(qry.record(0).value("xrg").toString());
    ui->textyrg->setText(qry.record(0).value("yrg").toString());
    ui->textzrg->setText(qry.record(0).value("zrg").toString());
    ui->textxpolrg->setText(qry.record(0).value("xpolrg").toString());
    ui->textypolrg->setText(qry.record(0).value("ypolrg").toString());
    ui->textzpolrg->setText(qry.record(0).value("zpolrg").toString());
    ui->textvxrg->setText(qry.record(0).value("vxrg").toString());
    ui->textvyrg->setText(qry.record(0).value("vyrg").toString());
    ui->textvzrg->setText(qry.record(0).value("vzrg").toString());
    ui->texttemprg->setText(qry.record(0).value("temprg").toString());
    ui->textdensrg->setText(qry.record(0).value("densrg").toString());
    ui->textanerg->setText(qry.record(0).value("anerg").toString());
    ui->textvtrbrg->setText(qry.record(0).value("vtrbrg").toString());
//    ui->textitrg->setText(qry.record(0).value("itrg").toString());
    ui->textedenrg->setText(qry.record(0).value("edenrg").toString());
    ui->textdstdrg->setText(qry.record(0).value("dstdrg").toString());
    ui->textede2rg->setText(qry.record(0).value("ede2rg").toString());
    ui->textdst2rg->setText(qry.record(0).value("dst2rg").toString());
    ui->textdsttrg->setText(qry.record(0).value("dsttrg").toString());
    ui->textadisc->setText(qry.record(0).value("adisc").toString());
    ui->textrindc->setText(qry.record(0).value("rindc").toString());
    ui->textroutdc->setText(qry.record(0).value("routdc").toString());
    ui->textemdc->setText(qry.record(0).value("emdc").toString());
    ui->textrdc->setText(qry.record(0).value("rdc").toString());
    ui->textxdc->setText(qry.record(0).value("xdc").toString());
    ui->textydc->setText(qry.record(0).value("ydc").toString());
    ui->textzdc->setText(qry.record(0).value("zdc").toString());
    ui->textxdisc->setText(qry.record(0).value("xdisc").toString());
    ui->textydisc->setText(qry.record(0).value("ydisc").toString());
    ui->textzdisc->setText(qry.record(0).value("zdisc").toString());
    ui->textvxdc->setText(qry.record(0).value("vxdc").toString());
    ui->textvydc->setText(qry.record(0).value("vydc").toString());
    ui->textvzdc->setText(qry.record(0).value("vzdc").toString());
    ui->texttempdc->setText(qry.record(0).value("tempdc").toString());
    ui->textdensdc->setText(qry.record(0).value("densdc").toString());
    ui->textanedc->setText(qry.record(0).value("anedc").toString());
    ui->textvtrbdc->setText(qry.record(0).value("vtrbdc").toString());
    ui->textedendc->setText(qry.record(0).value("edendc").toString());
//    ui->textitdc->setText(qry.record(0).value("itdc").toString());
    ui->textetmpdc->setText(qry.record(0).value("etmpdc").toString());
    ui->textdstddc->setText(qry.record(0).value("dstddc").toString());
    ui->textdsttdc->setText(qry.record(0).value("dsttdc").toString());
    ui->textaneb->setText(qry.record(0).value("aneb").toString());
    ui->textrinnb->setText(qry.record(0).value("rinnb").toString());
    ui->textroutnb->setText(qry.record(0).value("routnb").toString());
    ui->textemnb->setText(qry.record(0).value("emnb").toString());
    ui->textrnb->setText(qry.record(0).value("rnb").toString());
    ui->texthinvnb->setText(qry.record(0).value("hinvnb").toString());
    ui->texttinvnb->setText(qry.record(0).value("tinvnb").toString());
    ui->texthwindnb->setText(qry.record(0).value("hwindnb").toString());
//    ui->textidennb->setText(qry.record(0).value("idennb").toString());
    ui->textxneb->setText(qry.record(0).value("xneb").toString());
    ui->textyneb->setText(qry.record(0).value("yneb").toString());
    ui->textzneb->setText(qry.record(0).value("zneb").toString());
    ui->textvxnb->setText(qry.record(0).value("vxnb").toString());
    ui->textvynb->setText(qry.record(0).value("vynb").toString());
    ui->textvznb->setText(qry.record(0).value("vznb").toString());
    ui->texttempnb->setText(qry.record(0).value("tempnb").toString());
    ui->textdensnb->setText(qry.record(0).value("densnb").toString());
    ui->textanenb->setText(qry.record(0).value("anenb").toString());
    ui->textvtrbnb->setText(qry.record(0).value("vtrbnb").toString());
    ui->textedennb->setText(qry.record(0).value("edennb").toString());
//    ui->textitnb->setText(qry.record(0).value("itnb").toString());
    ui->textetmpnb->setText(qry.record(0).value("etmpnb").toString());
    ui->textdstdnb->setText(qry.record(0).value("dstdnb").toString());
    ui->textdsttnb->setText(qry.record(0).value("dsttnb").toString());
    ui->textv1fw->setText(qry.record(0).value("v1fw").toString());
    ui->textv2fw->setText(qry.record(0).value("v2fw").toString());
    ui->textr1fw->setText(qry.record(0).value("r1fw").toString());
    ui->textr2fw->setText(qry.record(0).value("r2fw").toString());
    ui->textx1fw->setText(qry.record(0).value("x1fw").toString());
    ui->texty1fw->setText(qry.record(0).value("y1fw").toString());
    ui->textz1fw->setText(qry.record(0).value("z1fw").toString());
    ui->textx2fw->setText(qry.record(0).value("x2fw").toString());
    ui->texty2fw->setText(qry.record(0).value("y2fw").toString());
    ui->textz2fw->setText(qry.record(0).value("z2fw").toString());
    ui->textvxfw->setText(qry.record(0).value("vxfw").toString());
    ui->textvyfw->setText(qry.record(0).value("vyfw").toString());
    ui->textvzfw->setText(qry.record(0).value("vzfw").toString());
    ui->textxfw->setText(qry.record(0).value("xfw").toString());
    ui->textyfw->setText(qry.record(0).value("yfw").toString());
    ui->textzfw->setText(qry.record(0).value("zfw").toString());
    ui->textpfw->setText(qry.record(0).value("pfw").toString());
    ui->texttempfw->setText(qry.record(0).value("tempfw").toString());
    ui->textdensfw->setText(qry.record(0).value("densfw").toString());
    ui->textanefw->setText(qry.record(0).value("anefw").toString());
    ui->textvtrbfw->setText(qry.record(0).value("vtrbfw").toString());
    ui->textedenfw->setText(qry.record(0).value("edenfw").toString());
    ui->textdstdfw->setText(qry.record(0).value("dstdfw").toString());
    ui->textdsttfw->setText(qry.record(0).value("dsttfw").toString());
    ui->textajet->setText(qry.record(0).value("ajet").toString());
    ui->textrinjt->setText(qry.record(0).value("rinjt").toString());
    ui->textroutjt->setText(qry.record(0).value("routjt").toString());
    ui->textvjt->setText(qry.record(0).value("vjt").toString());
    ui->textxjet->setText(qry.record(0).value("xjet").toString());
    ui->textyjet->setText(qry.record(0).value("yjet").toString());
    ui->textzjet->setText(qry.record(0).value("zjet").toString());
    ui->textvxjt->setText(qry.record(0).value("vxjt").toString());
    ui->textvyjt->setText(qry.record(0).value("vyjt").toString());
    ui->textvzjt->setText(qry.record(0).value("vzjt").toString());
    ui->texttempjt->setText(qry.record(0).value("tempjt").toString());
    ui->textdensjt->setText(qry.record(0).value("densjt").toString());
    ui->textanejt->setText(qry.record(0).value("anejt").toString());
    ui->textvtrbjt->setText(qry.record(0).value("vtrbjt").toString());
    ui->textdstdjt->setText(qry.record(0).value("dstdjt").toString());
    ui->textdsttjt->setText(qry.record(0).value("dsttjt").toString());
    ui->textaufo->setText(qry.record(0).value("aufo").toString());
    ui->textrinuf->setText(qry.record(0).value("rinuf").toString());
    ui->textroutuf->setText(qry.record(0).value("routuf").toString());
    ui->textemuf->setText(qry.record(0).value("emuf").toString());
    ui->textruf->setText(qry.record(0).value("ruf").toString());
    ui->textxuf->setText(qry.record(0).value("xuf").toString());
    ui->textyuf->setText(qry.record(0).value("yuf").toString());
    ui->textzuf->setText(qry.record(0).value("zuf").toString());
    ui->textxufo->setText(qry.record(0).value("xufo").toString());
    ui->textyufo->setText(qry.record(0).value("yufo").toString());
    ui->textzufo->setText(qry.record(0).value("zufo").toString());
    ui->textvxuf->setText(qry.record(0).value("vxuf").toString());
    ui->textvyuf->setText(qry.record(0).value("vyuf").toString());
    ui->textvzuf->setText(qry.record(0).value("vzuf").toString());
    ui->texttempuf->setText(qry.record(0).value("tempuf").toString());
    ui->textdensuf->setText(qry.record(0).value("densuf").toString());
    ui->textaneuf->setText(qry.record(0).value("aneuf").toString());
    ui->textvtrbuf->setText(qry.record(0).value("vtrbuf").toString());
    ui->textedenuf->setText(qry.record(0).value("edenuf").toString());
//    ui->textituf->setText(qry.record(0).value("ituf").toString());
    ui->textetmpuf->setText(qry.record(0).value("etmpuf").toString());
    ui->textdstduf->setText(qry.record(0).value("dstduf").toString());
    ui->textdsttuf->setText(qry.record(0).value("dsttuf").toString());
    ui->textrinsh->setText(qry.record(0).value("rinsh").toString());
    ui->textroutsh->setText(qry.record(0).value("routsh").toString());
    ui->textvsh->setText(qry.record(0).value("vsh").toString());
    ui->textevelsh->setText(qry.record(0).value("evelsh").toString());
    ui->textrcsh->setText(qry.record(0).value("rcsh").toString());
    ui->textvxsh->setText(qry.record(0).value("vxsh").toString());
    ui->textvysh->setText(qry.record(0).value("vysh").toString());
    ui->textvzsh->setText(qry.record(0).value("vzsh").toString());
    ui->texttempsh->setText(qry.record(0).value("tempsh").toString());
    ui->textdenssh->setText(qry.record(0).value("denssh").toString());
    ui->textanesh->setText(qry.record(0).value("anesh").toString());
    ui->textvtrbsh->setText(qry.record(0).value("vtrbsh").toString());
    ui->textdstdsh->setText(qry.record(0).value("dstdsh").toString());
    ui->textdsttsh->setText(qry.record(0).value("dsttsh").toString());
//    ui->texttemp0->setText(qry.record(0).value("temp0").toString());
//    ui->textdens0->setText(qry.record(0).value("dens0").toString());
//    ui->textane0->setText(qry.record(0).value("ane0").toString());
//    ui->textv0->setText(qry.record(0).value("v0").toString());

    connClose();
}

void shellspec::on_buttonAbundances_clicked()
{
    if ( ui->textAbundances->isEnabled() ) {
        QString directory = QFileDialog::getOpenFileName(this, tr("Find Files"), QDir::currentPath());
        ui->textAbundances->setText(directory);
    } else {
    QMessageBox msgBox;
    msgBox.setText("Desactive Solar Abundances first!.");
    msgBox.exec();
    }
}

bool shellspec::create_table_only_once()
{
    int created_succefull = false;

    QString cmdt = "CREATE TABLE user_inputs "
                  "(ID INT PRIMARY KEY, FIELD text NOT NULL, alam1 FLOAT(9,2) NOT NULL, "
                  "alamn FLOAT(9,2) NOT NULL, alams FLOAT(6,2) NOT NULL, loglam INT NOT NULL, "
                  "cutoff FLOAT NOT NULL, imodel INT NOT NULL, irotat INT NOT NULL, ipart INT NOT NULL, "
                  "ichemc INT NOT NULL, ielnd INT NOT NULL, ithom INT NOT NULL, irayl INT NOT NULL, "
                  "imie INT NOT NULL, imiepf INT NOT NULL, ihyd INT NOT NULL, iopac INT NOT NULL, "
                  "iline INT NOT NULL, eps FLOAT NOT NULL, ionu INT NOT NULL, ior INT NOT NULL, iot INT NOT NULL, "
                  "offset FLOAT(4,2) NOT NULL, phase1 FLOAT NOT NULL, phasen FLOAT NOT NULL, nphase INT NOT NULL, "
                  "dinc FLOAT(4,2) NOT NULL, dd FLOAT(8,3) NOT NULL, lunt1 INT NOT NULL, xunt1 FLOAT NOT NULL, "
                  "yunt1 FLOAT NOT NULL, lunt2 INT NOT NULL, xunt2 FLOAT NOT NULL, yunt2 FLOAT NOT NULL, "
                  "lunt3 INT NOT NULL, xunt3 FLOAT NOT NULL, yunt3 FLOAT NOT NULL, rmdfx1 FLOAT NOT NULL, "
                  "rmdfx2 FLOAT NOT NULL, rmdfy1 FLOAT NOT NULL, rmdfy2 FLOAT NOT NULL, rmdfz1 FLOAT NOT NULL, "
                  "rmdfz2 FLOAT NOT NULL, rmdfx3 FLOAT NOT NULL, rmdfx4 FLOAT NOT NULL, stepf FLOAT NOT NULL, "
                  "stepfz FLOAT NOT NULL, gainfx FLOAT NOT NULL, gainfy FLOAT NOT NULL, gainfz FLOAT NOT NULL, "
                  "rmdx1 FLOAT(5,3) NOT NULL, rmdx2 FLOAT(5,3) NOT NULL, rmdy1 FLOAT(5,3) NOT NULL, "
                  "rmdy2 FLOAT(5,3) NOT NULL, rmdz1 FLOAT(5,3) NOT NULL, rmdz2 FLOAT(5,3) NOT NULL, "
                  "rmdz3 FLOAT(5,3) NOT NULL, rmdz4 FLOAT(5,3) NOT NULL, steps FLOAT(5,3) NOT NULL, "
                  "stepsz FLOAT(5,3) NOT NULL, gainx FLOAT(5,3) NOT NULL, gainy FLOAT(5,3) NOT NULL, "
                  "gainz FLOAT(5,3) NOT NULL, ISTAR INT NOT NULL, ICOMP INT NOT NULL, IENV INT NOT NULL, "
                  "ISPOT INT NOT NULL, ISM INT NOT NULL, IRING INT NOT NULL, IDISC INT NOT NULL, "
                  "INEBL INT NOT NULL, IFLOW INT NOT NULL, IJET INT NOT NULL, IUFO INT NOT NULL, "
                  "ISHELL INT NOT NULL, rstar FLOAT NOT NULL, Tstar FLOAT NOT NULL, eMstar FLOAT NOT NULL, "
                  "xstar FLOAT NOT NULL, ystar FLOAT NOT NULL, zstar FLOAT NOT NULL, vrotst FLOAT NOT NULL, "
                  "idifst INT NOT NULL, drotst FLOAT NOT NULL, hst FLOAT NOT NULL, vxst FLOAT NOT NULL, "
                  "vyst FLOAT NOT NULL, vzst FLOAT NOT NULL, dlst FLOAT NOT NULL, dlst2 FLOAT NOT NULL, "
                  "dgst FLOAT NOT NULL, ffst FLOAT NOT NULL, irrst INT NOT NULL, ialbst INT NOT NULL, "
                  "albst FLOAT NOT NULL, htst FLOAT NOT NULL, htsta FLOAT NOT NULL, ispst INT NOT NULL, "
                  "xspst FLOAT NOT NULL, yspst FLOAT NOT NULL, zspst FLOAT NOT NULL, aspst FLOAT NOT NULL, "
                  "tspst FLOAT NOT NULL, rcp FLOAT NOT NULL, tempcp FLOAT NOT NULL, qq FLOAT NOT NULL, "
                  "vrxcp FLOAT NOT NULL, vrycp FLOAT NOT NULL, vrzcp FLOAT NOT NULL, vrotcp FLOAT NOT NULL, "
                  "xcp FLOAT NOT NULL, ycp FLOAT NOT NULL, zcp FLOAT NOT NULL, vxcp FLOAT NOT NULL, "
                  "vycp FLOAT NOT NULL, vzcp FLOAT NOT NULL, dlcp FLOAT NOT NULL, dlcp2 FLOAT NOT NULL, "
                  "dgcp FLOAT NOT NULL, ffcp FLOAT NOT NULL, irrcp INT NOT NULL, ialbcp INT NOT NULL, "
                  "albcp FLOAT NOT NULL, htcp FLOAT NOT NULL, htcpa FLOAT NOT NULL, emen FLOAT NOT NULL, "
                  "qqen FLOAT NOT NULL, aen FLOAT NOT NULL, ffen FLOAT NOT NULL, hen FLOAT NOT NULL, "
                  "tempen FLOAT NOT NULL, densen FLOAT NOT NULL, aneen FLOAT NOT NULL, vtrben FLOAT NOT NULL, "
                  "dstden FLOAT NOT NULL, "
                  "dstten FLOAT NOT NULL, vrxsp FLOAT NOT NULL, vrysp FLOAT NOT NULL, vrzsp FLOAT NOT NULL, "
                  "vrotsp FLOAT NOT NULL, rsp FLOAT NOT NULL, xsp FLOAT NOT NULL, ysp FLOAT NOT NULL, "
                  "zsp FLOAT NOT NULL, vxsp FLOAT NOT NULL, vysp FLOAT NOT NULL, vzsp FLOAT NOT NULL, "
                  "tempsp FLOAT NOT NULL, denssp FLOAT NOT NULL, anesp FLOAT NOT NULL, vtrbsp FLOAT NOT NULL, "
                  "dstdsp FLOAT NOT NULL, dsttsp FLOAT NOT NULL, v1sm FLOAT NOT NULL, v2sm FLOAT NOT NULL, "
                  "r1sm FLOAT NOT NULL, r2sm FLOAT NOT NULL, x1sm FLOAT NOT NULL, y1sm FLOAT NOT NULL, "
                  "z1sm FLOAT NOT NULL, x2sm FLOAT NOT NULL, y2sm FLOAT NOT NULL, z2sm FLOAT NOT NULL, "
                  "vxsm FLOAT NOT NULL, vysm FLOAT NOT NULL, vzsm FLOAT NOT NULL, xsm FLOAT NOT NULL, "
                  "ysm FLOAT NOT NULL, zsm FLOAT NOT NULL, psm FLOAT NOT NULL, tempsm FLOAT NOT NULL, "
                  "denssm FLOAT NOT NULL, anesm FLOAT NOT NULL, vtrbsm FLOAT NOT NULL, edensm FLOAT NOT NULL, "
                  "dstdsm FLOAT NOT NULL, dsttsm FLOAT NOT NULL, rrg FLOAT NOT NULL, emrg FLOAT NOT NULL, "
                  "b1rg FLOAT NOT NULL, b2rg FLOAT NOT NULL, a1rg FLOAT NOT NULL, a2rg FLOAT NOT NULL, "
                  "dr1rg FLOAT NOT NULL, dr2rg FLOAT NOT NULL, xrg FLOAT NOT NULL, yrg FLOAT NOT NULL, "
                  "zrg FLOAT NOT NULL, xpolrg FLOAT NOT NULL, ypolrg FLOAT NOT NULL, zpolrg FLOAT NOT NULL, "
                  "vxrg FLOAT NOT NULL, vyrg FLOAT NOT NULL, vzrg FLOAT NOT NULL, temprg FLOAT NOT NULL, "
                  "densrg FLOAT NOT NULL, anerg FLOAT NOT NULL, vtrbrg FLOAT NOT NULL, itrg FLOAT NOT NULL, "
                  "edenrg FLOAT NOT NULL, dstdrg FLOAT NOT NULL, ede2rg FLOAT NOT NULL, dst2rg FLOAT NOT NULL, "
                  "dsttrg FLOAT NOT NULL, adisc FLOAT NOT NULL, rindc FLOAT NOT NULL, routdc FLOAT NOT NULL, "
                  "emdc FLOAT NOT NULL, rdc FLOAT NOT NULL, xdc FLOAT NOT NULL, ydc FLOAT NOT NULL, "
                  "zdc FLOAT NOT NULL, xdisc FLOAT NOT NULL, ydisc FLOAT NOT NULL, zdisc FLOAT NOT NULL, "
                  "vxdc FLOAT NOT NULL, vydc FLOAT NOT NULL, vzdc FLOAT NOT NULL, tempdc FLOAT NOT NULL, "
                  "densdc FLOAT NOT NULL, anedc FLOAT NOT NULL, vtrbdc FLOAT NOT NULL, edendc FLOAT NOT NULL, "
                  "itdc INT NOT NULL, etmpdc FLOAT NOT NULL, dstddc FLOAT NOT NULL, dsttdc FLOAT NOT NULL, "
                  "aneb FLOAT NOT NULL, rinnb FLOAT NOT NULL, routnb FLOAT NOT NULL, emnb FLOAT NOT NULL, "
                  "rnb FLOAT NOT NULL, hinvnb DOUBLE NOT NULL, tinvnb DOUBLE NOT NULL, hwindnb DOUBLE NOT NULL, "
                  "idennb INT NOT NULL, xneb FLOAT NOT NULL, yneb FLOAT NOT NULL, zneb FLOAT NOT NULL, "
                  "vxnb FLOAT NOT NULL, vynb FLOAT NOT NULL, vznb FLOAT NOT NULL, tempnb FLOAT NOT NULL, "
                  "densnb FLOAT NOT NULL, anenb FLOAT NOT NULL, vtrbnb FLOAT NOT NULL, edennb FLOAT NOT NULL, "
                  "itnb INT NOT NULL, etmpnb FLOAT NOT NULL, dstdnb FLOAT NOT NULL, dsttnb FLOAT NOT NULL, "
                  "v1fw FLOAT NOT NULL, v2fw FLOAT NOT NULL, r1fw FLOAT NOT NULL, r2fw FLOAT NOT NULL, "
                  "x1fw FLOAT NOT NULL, y1fw FLOAT NOT NULL, z1fw FLOAT NOT NULL, x2fw FLOAT NOT NULL, "
                  "y2fw FLOAT NOT NULL, z2fw FLOAT NOT NULL, vxfw FLOAT NOT NULL, vyfw FLOAT NOT NULL, "
                  "vzfw FLOAT NOT NULL, xfw FLOAT NOT NULL, yfw FLOAT NOT NULL, zfw FLOAT NOT NULL, "
                  "pfw FLOAT NOT NULL, tempfw FLOAT NOT NULL, densfw FLOAT NOT NULL, anefw FLOAT NOT NULL, "
                  "vtrbfw FLOAT NOT NULL, edenfw FLOAT NOT NULL, dstdfw FLOAT NOT NULL, dsttfw FLOAT NOT NULL, "
                  "ajet FLOAT NOT NULL, rinjt FLOAT NOT NULL, routjt FLOAT NOT NULL, vjt FLOAT NOT NULL, "
                  "xjet FLOAT NOT NULL, yjet FLOAT NOT NULL, zjet FLOAT NOT NULL, vxjt FLOAT NOT NULL, "
                  "vyjt FLOAT NOT NULL, vzjt FLOAT NOT NULL, tempjt FLOAT NOT NULL, densjt FLOAT NOT NULL, "
                  "anejt FLOAT NOT NULL, vtrbjt FLOAT NOT NULL, dstdjt FLOAT NOT NULL, dsttjt FLOAT NOT NULL, "
                  "aufo FLOAT NOT NULL, rinuf FLOAT NOT NULL, routuf FLOAT NOT NULL, emuf FLOAT NOT NULL, "
                  "ruf FLOAT NOT NULL, xuf FLOAT NOT NULL, yuf FLOAT NOT NULL, zuf FLOAT NOT NULL, "
                  "xufo FLOAT NOT NULL, yufo FLOAT NOT NULL, zufo FLOAT NOT NULL, vxuf FLOAT NOT NULL, "
                  "vyuf FLOAT NOT NULL, vzuf FLOAT NOT NULL, tempuf FLOAT NOT NULL, densuf FLOAT NOT NULL, "
                  "aneuf FLOAT NOT NULL, vtrbuf FLOAT NOT NULL, edenuf FLOAT NOT NULL, ituf INT NOT NULL, "
                  "etmpuf FLOAT NOT NULL, dstduf FLOAT NOT NULL, dsttuf FLOAT NOT NULL, rinsh FLOAT NOT NULL, "
                  "routsh FLOAT NOT NULL, vsh FLOAT NOT NULL, evelsh FLOAT NOT NULL, rcsh FLOAT NOT NULL, "
                  "vxsh FLOAT NOT NULL, vysh FLOAT NOT NULL, vzsh FLOAT NOT NULL, tempsh FLOAT NOT NULL, "
                  "denssh FLOAT NOT NULL, anesh FLOAT NOT NULL, vtrbsh FLOAT NOT NULL, dstdsh FLOAT NOT NULL, "
                  "dsttsh FLOAT NOT NULL, temp0 FLOAT NOT NULL, dens0 FLOAT NOT NULL, ane0 FLOAT NOT NULL, "
                  "v0 FLOAT NOT NULL, abund_txt TEXT, ins1_txt TEXT, ins2_txt TEXT, ins3_txt TEXT, "
                   "lineopac_txt TEXT, miescat_txt TEXT, phasefunc_txt TEXT, phasesfile TEXT, prefix TEXT)";
    QString cmdf = "INSERT INTO user_inputs (ID, FIELD, alam1, alamn, alams, loglam, cutoff, imodel, irotat, ipart, ichemc, ielnd, ithom, irayl, imie, imiepf, ihyd, iopac, iline, eps, ionu, ior, iot, offset, phase1, phasen, nphase, dinc, dd, lunt1, xunt1, yunt1, lunt2, xunt2, yunt2, lunt3, xunt3, yunt3, rmdfx1, rmdfx2, rmdfy1, rmdfy2, rmdfz1, rmdfz2, rmdfx3, rmdfx4, stepf, stepfz, gainfx, gainfy, gainfz, rmdx1, rmdx2, rmdy1, rmdy2, rmdz1, rmdz2, rmdz3, rmdz4, steps, stepsz, gainx, gainy, gainz, ISTAR, ICOMP, IENV, ISPOT, ISM, IRING, IDISC, INEBL, IFLOW, IJET, IUFO, ISHELL, rstar, Tstar, eMstar, xstar, ystar, zstar, vrotst, idifst, drotst, hst, vxst, vyst, vzst, dlst, dlst2, dgst, ffst, irrst, ialbst, albst, htst, htsta, ispst, xspst, yspst, zspst, aspst, tspst, rcp, tempcp, qq, vrxcp, vrycp, vrzcp, vrotcp, xcp, ycp, zcp, vxcp, vycp, vzcp, dlcp, dlcp2, dgcp, ffcp, irrcp, ialbcp, albcp, htcp, htcpa, emen, qqen, aen, ffen, hen, tempen, densen, aneen, vtrben, dstden, dstten, vrxsp, vrysp, vrzsp, vrotsp, rsp, xsp, ysp, zsp, vxsp, vysp, vzsp, tempsp, denssp, anesp, vtrbsp, dstdsp, dsttsp, v1sm, v2sm, r1sm, r2sm, x1sm, y1sm, z1sm, x2sm, y2sm, z2sm, vxsm, vysm, vzsm, xsm, ysm, zsm, psm, tempsm, denssm, anesm, vtrbsm, edensm, dstdsm, dsttsm, rrg, emrg, b1rg, b2rg, a1rg, a2rg, dr1rg, dr2rg, xrg, yrg, zrg, xpolrg, ypolrg, zpolrg, vxrg, vyrg, vzrg, temprg, densrg, anerg, vtrbrg, itrg, edenrg, dstdrg, ede2rg, dst2rg, dsttrg, adisc, rindc, routdc, emdc, rdc, xdc, ydc, zdc, xdisc, ydisc, zdisc, vxdc, vydc, vzdc, tempdc, densdc, anedc, vtrbdc, edendc, itdc, etmpdc, dstddc, dsttdc, aneb, rinnb, routnb, emnb, rnb, hinvnb, tinvnb, hwindnb, idennb, xneb, yneb, zneb, vxnb, vynb, vznb, tempnb, densnb, anenb, vtrbnb, edennb, itnb, etmpnb, dstdnb, dsttnb, v1fw, v2fw, r1fw, r2fw, x1fw, y1fw, z1fw, x2fw, y2fw, z2fw, vxfw, vyfw, vzfw, xfw, yfw, zfw, pfw, tempfw, densfw, anefw, vtrbfw, edenfw, dstdfw, dsttfw, ajet, rinjt, routjt, vjt, xjet, yjet, zjet, vxjt, vyjt, vzjt, tempjt, densjt, anejt, vtrbjt, dstdjt, dsttjt, aufo, rinuf, routuf, emuf, ruf, xuf, yuf, zuf, xufo, yufo, zufo, vxuf, vyuf, vzuf, tempuf, densuf, aneuf, vtrbuf, edenuf, ituf, etmpuf, dstduf, dsttuf, rinsh, routsh, vsh, evelsh, rcsh, vxsh, vysh, vzsh, tempsh, denssh, anesh, vtrbsh, dstdsh, dsttsh, temp0, dens0, ane0, v0) "
                   "VALUES (2, 'user', 20000.0, 300000.0, 5000.0, 1, 0.0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1.0, 0, 0, 0, 0, 0, 360, 21, 90.0, 500.0, 0, 1.0, 1.0, 0, 1.0, 1.0, 0, 1.0, 1.0, -5.0, 5.0, -5.0, 5.0, -5.0, 5.0, 0.0, 0.0, 0.2, 0.2, 1.0, 1.0, 1.0, -5.0, 5.0, -5.0, 5.0, -5.0, 5.0, 0.0, 0.0, 0.2, 0.2, 1.0, 1.0, 1.0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1.56, 7000.0, 1.578, 0.0, 0.0, 1.0, 180.0, 0, 0.9, 0.1, 0.0, -30.3, 0.0, 0.2656, 0.3917, 0.08, 1.0, 0, 0, 0.0, 1.0, 1.0, 1, -1.0, 0.0, 1.0, 10.0, 0.9, 2.0, 8000.0, 10.0, 0.0, 0.0, 1.0, 0.0, -10.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0, 0, 0.0, 1.0, 1.0, 1.578, 0.099, 2.92, 1.3, 0.25, 251.0, 1e-18, 21000000000.0, 0.0, 9e-17, 7050.0, 0.0, 0.0, 1.0, 0.0, 2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1400.0, 1e-19, 21000000000.0, 0.0, 1e-16, 1400.0, 100.0, 100.0, 0.2, 0.2, 0.0, -0.2, 0.0, 0.0, 0.2, 0.0, 0.0, 306.4, 0.0, 0.0, 0.0, 0.0, 0.0, 8000.0, 0.00011, 21000000000.0, 10.0, 0.0, 1e-14, 1400.0, 2.8, 0.7, 360.0, 280.0, 0.1, 0.3, 0.1, 0.3, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.1, 4.65e-15, 10.0, 0.0, 2, -25, 4.65e-15, -6.0, 4.3e-16, 2100.0, 0.3, 20.0, 200.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 885.0, 7.5e-07, 1000000000000000.0, 0.0, -1.5, 3, -0.5, 7.5e-09, 885.0, 7.0, 20.0, 200.0, 1.0, 1.0, 6.0, 6.0, 8.8, 0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 885.0, 7.5e-07, 1500000000000000.0, 0.0, -1.5, 3, -0.5, 7.5e-09, 885.0, 100.0, 100.0, 0.2, 0.2, 2.92, -0.2, 0.0, 2.92, 0.2, 0.0, 0.0, -30.3, 0.0, 0.0, 0.0, 1.0, 0.4387, 8000.0, 1.1e-14, 21000000000.0, 10.0, 0.0, 1e-29, 8000.0, 20.0, 10.0, 15.0, 600.0, 0.0, 0.0, 1.0, 0.0, -46.0, 0.0, 8000.0, 1e-13, 1000000000.0, 260.0, 1e-30, 8000.0, 2.0, 2.0, 6.0, 2.63, 1.95, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, -200.0, 0.0, 8000.0, 3e-14, 2100000.0, 20.0, 0.0, 1, -0.5, 5.7e-30, 8000.0, 10.0, 15.0, 1.0, 0.0, 4.4, 0.0, -46.0, 0.0, 9000.0, 3e-14, 21000000000.0, 500.0, 1e-30, 9000.0, 4000.0, 0.0, 10000000.0, 0.0)";

    connOpen();
    QSqlQuery qry;
    QSqlQueryModel qry2;

    qry2.setQuery("SELECT * FROM user_inputs WHERE ID=2");

    if (qry2.lastError().isValid()){
        qry.prepare(cmdt);

        if (qry.exec()){
            qry.prepare(cmdf);
            qDebug()<<"Table created correctly";
            if (qry.exec()){
                created_succefull = true;
                qDebug()<<"Default values inserted correctly";
            }
        }
    }
    connClose();
    return created_succefull;
}

QString shellspec::get_prefix_model()
{
    QString prefix = ui->textPrefix->text();
    return prefix;
}

void shellspec::rename_all_output_files()
{
    QString prefix = get_prefix_model();
    qDebug()<<prefix;
    const QString SS_DIR = QDir::currentPath()+QDir::separator();
    QString shellspectrum = SS_DIR + prefix + "shellspectrum";
    QString lightcurve = SS_DIR + prefix + "lightcurve";

    rename_one_file(SS_DIR + "shellspectrum", shellspectrum);
    rename_one_file(SS_DIR + "lightcurve", lightcurve);

    int nfort = count_total_graphs_current_model();

    for (int i = 1; i < nfort+1; ++i) {
        QString currentfort = tr("%1").arg(i, 3, 10, QChar('0'));
        QString fortfile = SS_DIR + prefix + "2Dimage_" + currentfort;
        QFile file(fortfile);
        rename_one_file(SS_DIR + "2Dimage_" + currentfort, fortfile);
    }
}

void rename_one_file(QString inFile, QString outFile)
{
    QFile file(inFile);
    if (!file.rename(outFile)){
        file.remove(outFile);
        file.rename(outFile);
    }
}

int shellspec::count_total_graphs_current_model()
{
    const QString SS_DIR = QDir::currentPath()+QDir::separator();
    connOpen();
    QSqlQueryModel qry;
    qry.setQuery("SELECT prefix FROM user_inputs WHERE ID=2");
    QString prefix = qry.record(0).value("prefix").toString();
    connClose();
    // QString prefix = get_prefix_model();
//    qDebug()<<"shellspec:"<<prefix;
    int total_graphs = 0;
    QString ssFile = SS_DIR + prefix + "shellspectrum";
//    qDebug()<<ssFile;
    QFile file(ssFile);
    if (file.open(QIODevice::ReadOnly)){
        QTextStream st(&file);
        QString line = st.readLine();
        while (!line.isNull()){
            QStringList elems = line.split(QRegularExpression("\\s+"));
            if (elems.size() == 1){
                total_graphs += 1;
            }
            line = st.readLine();
        }
    }
    return total_graphs;
}

void shellspec::on_actionVersion_triggered()
{
    About_version abversion;
    abversion.setModal(true);
    abversion.exec();
}
