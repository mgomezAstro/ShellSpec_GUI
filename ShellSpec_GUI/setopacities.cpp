#include "setopacities.h"
#include "ui_setopacities.h"
#include "QFileDialog"
#include "QDir"
#include "QMessageBox"
#include "QTextStream"
#include "QFile"


setOpacities::setOpacities(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::setOpacities)
{
    ui->setupUi(this);

    // Get the USER saved inputs.
    QString field = "user";
    set_default_user_inputs(field);
}

setOpacities::~setOpacities()
{
    delete ui;
}

void setOpacities::on_buttonLineOP_clicked()
{
    if ( ui->textLineOp->isEnabled() ) {
        QString directory = QFileDialog::getOpenFileName(this, tr("Find Files"), QDir::currentPath());
        ui->textLineOp->setText(directory);
    } else {
        QMessageBox msgBox;
        msgBox.setText("Activate Line Opacities first!.");
        msgBox.exec();
    }
}

void setOpacities::on_buttonMS_clicked()
{
    if ( ui->textMS->isEnabled() ) {
        QString directory = QFileDialog::getOpenFileName(this, tr("Find Files"), QDir::currentPath());
        ui->textMS->setText(directory);
    } else {
    QMessageBox msgBox;
    msgBox.setText("Activate Mie Scattering first!.");
    msgBox.exec();
    }
}

void setOpacities::on_buttonOpacities_accepted()
{
    const QString SS_DIR = QDir::currentPath()+QDir::separator();
    std::string dir_to_LineOp;
    int iline = 0;
    int ithom = 0;
    int iray = 0;
    int imie = 1;
    int imiepf = 1;
    int ihyd = 1;
    int iopac = 0;
    float eps = 1.0;
    QString lineopac_txt, miescat_txt, phasefunc_txt;


    // Get Thomson Scattering value
    if (ui->checkTS->isChecked()) {
        ithom = 1;
    } else {
        ithom =0;
    }

    // Get Rayleight Scattering value
    if (ui->checkRS->isChecked()){
        iray = 1;
    } else {
        iray = 0;
    }

    // Get Mie Scaterring value
    if (ui->checkMS->isChecked()){
        QString dir_to_MS = ui->textMS->text();
        QFileInfo check_file(dir_to_MS);
        if (check_file.exists() && check_file.isFile()){
            QString imie_model = ui->comboMS->currentText();
            QFile::copy(dir_to_MS, SS_DIR+"dust_opac");
            miescat_txt = dir_to_MS;
            if (imie_model == "Option 1"){
                imie = 1;
            } else if (imie_model == "Option 2") {
                imie = 2;
            } else {
                imie = 3;
            }
        } else {
//            QString imie_model = ui->comboMS->currentText();
//            QFile::copy(SS_DIR+"dust_opac.default", SS_DIR+"dust_opac");
//            if (imie_model == "Option 1"){
//                imie = 1;
//            } else if (imie_model == "Option 2") {
//                imie = 2;
//            } else {
//                imie = 3;
//            }
            imie = 0;
            miescat_txt = "";
            ui->checkMS->setChecked(false);
            QMessageBox msgBox;
            msgBox.warning(this, "MIE input file warning.", "No such file or directory:" + dir_to_MS + ".");
        }
        // Check for Phase Function as long as Mie Scattering is Activate.
        if (ui->checkMiePF->isChecked()){
            QString dir_to_MiePF = ui->textMiePF->text();
            QFileInfo check_file(dir_to_MiePF);
            if (check_file.exists() && check_file.isFile()){
                 imiepf = 1;
                 QFile::copy(dir_to_MiePF, SS_DIR+"mie_phase");
                 phasefunc_txt = dir_to_MiePF;
            } else {
                imiepf = 0;
                phasefunc_txt = "";
//                QFile::copy(SS_DIR+"mie_phase.default", SS_DIR+"mie_phase");
                QMessageBox msgBox;
                msgBox.warning(this, "MIEPF input file warning.", "No such file or directory:" + dir_to_MiePF + ".");
            }
        } else {
            imiepf = 0;
            phasefunc_txt = "";
        }
    } else {
        imie = 0;
        imiepf = 0;
        miescat_txt = "";
        phasefunc_txt = "";
    }


    // Hydrogen bound-free and free-free Scaterring values
    if (ui->checkHbf_ff->isChecked()){
        ihyd = 1;
    } else {
        ihyd = 0;
    }

    // Gas True opacity tabulated.
    if (ui->checkExtratab->isChecked()){
        iopac = 1;
    } else {
        iopac = 0;
    }

    // Check Line Opacities values
    if ( (ui->checkLineOp->isChecked())){
        QString dir_to_LineOp = ui->textLineOp->text();
        QFileInfo check_file(dir_to_LineOp);
        if (check_file.exists() && check_file.isFile()){
            iline = 1;
            QFile::copy(dir_to_LineOp, SS_DIR+"line.dat");
            lineopac_txt = dir_to_LineOp;
        } else {
            iline = 0;
//            QFile::copy(SS_DIR+"line.dat.default", SS_DIR+"line.dat");
            ui->checkLineOp->setChecked(false);
            QMessageBox msgBox;
            msgBox.warning(this, "Line opacity input file warning.", "No such file or directory:" + dir_to_LineOp + ".");
        }
    } else {
        iline = 0;
        lineopac_txt = "";
    }

    // Check experimentl Split Line opacities values
    eps = 1.0;

    // Writing the final config file.
    shellspec conn;
    conn.connOpen();
    QSqlQuery qry;
    qry.prepare("UPDATE user_inputs "
                    "SET iline=:iline, ithom=:ithom, irayl=:irayl, imie=:imie,"
                    " imiepf=:idmiepf, ihyd=:ihyd, iopac=:iopac, eps=:eps,"
                " lineopac_txt=:lineopac_txt, miescat_txt=:miescat_txt, phasefunc_txt=:phasefunc_txt"
                    " WHERE ID=2");
        qry.bindValue(":iline", iline);
        qry.bindValue(":ithom", ithom);
        qry.bindValue(":irayl", iray);
        qry.bindValue(":imie", imie);
        qry.bindValue(":idmiepf", imiepf);
        qry.bindValue(":ihyd", ihyd);
        qry.bindValue(":iopac", iopac);
        qry.bindValue(":eps", eps);
        qry.bindValue(":lineopac_txt", lineopac_txt);
        qry.bindValue(":miescat_txt", miescat_txt);
        qry.bindValue(":phasefunc_txt", phasefunc_txt);

    // Executing the Query
    if (qry.exec()){
        qDebug()<<"Opacity DB:"<<"Executed correctly."<< endl;
        conn.connClose();
    } else {
        qDebug()<<"Opacity DB error:"<<qry.lastError().text()<<endl;
        qDebug()<<qry.executedQuery()<<endl;
        qDebug()<<qry.lastQuery()<<endl;
    }
}

void setOpacities::set_default_user_inputs(QString field){
    QString id = "1";
    if (field == "user"){
        id = "2";
    } else if (field == "default"){
        id = "1";
    }
    shellspec conn;
    conn.connOpen();
    QSqlQueryModel qry;
    qry.setQuery("SELECT iline, ithom, irayl, imie, imiepf, ihyd, iopac, eps,"
                 " lineopac_txt, miescat_txt, phasefunc_txt"
                " FROM user_inputs WHERE ID="+id);
    qDebug()<<"Opacity defaults DB errors: "<<qry.lastError()<<endl;
    if (qry.record(0).value("ithom").toInt() == 1){
        ui->checkTS->setChecked(true);
    } else {
        ui->checkTS->setChecked(false);
    }
    if (qry.record(0).value("irayl").toInt() == 1){
        ui->checkRS->setChecked(true);
    } else {
        ui->checkRS->setChecked(false);
    }
    if (qry.record(0).value("ihyd").toInt() == 1){
        ui->checkHbf_ff->setChecked(true);
    } else {
        ui->checkHbf_ff->setChecked(false);
    }
    if (qry.record(0).value("iopac").toInt() == 1){
        ui->checkExtratab->setChecked(true);
    } else {
        ui->checkExtratab->setChecked(false);
    }
    if (qry.record(0).value("iline").toInt() == 1){
        ui->checkLineOp->setChecked(true);
    } else {
        ui->checkLineOp->setChecked(false);
    }
    if (qry.record(0).value("imie").toInt() > 0){
        ui->checkMS->setChecked(true);
    } else {
        ui->checkMS->setChecked(false);
    }
    if (qry.record(0).value("imiepf").toInt() == 1){
        ui->checkMiePF->setChecked(true);
    } else {
        ui->checkMiePF->setChecked(false);
    }
    ui->textLineOp->insert(qry.record(0).value("lineopac_txt").toString());
    ui->textMS->setText(qry.record(0).value("miescat_txt").toString());
    ui->textMiePF->setText(qry.record(0).value("phasefunc_txt").toString());
    conn.connClose();
}
