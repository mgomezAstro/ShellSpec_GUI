#include "spectra.h"
#include "ui_spectra.h"
#include "QFileDialog"
#include "QMessageBox"

spectra::spectra(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::spectra)
{
    ui->setupUi(this);

    // Put defaults user.
    QString field = "user";
    set_default_user_inputs(field);
}

spectra::~spectra()
{
    delete ui;
}

void spectra::on_buttonSpectrum1_clicked()
{
    if ( ui->textSpectrum1->isEnabled() ) {
        QString directory = QFileDialog::getOpenFileName(this, tr("Find Files"), QDir::currentPath());
        ui->textSpectrum1->setText(directory);
    } else {
    QMessageBox msgBox;
    msgBox.setText("Active Spectrum1 first!.");
    msgBox.exec();
    }
}

void spectra::on_buttonSpectrum2_clicked()
{
    if ( ui->textSpectrum2->isEnabled() ) {
        QString directory = QFileDialog::getOpenFileName(this, tr("Find Files"), QDir::currentPath());
        ui->textSpectrum2->setText(directory);
    } else {
    QMessageBox msgBox;
    msgBox.setText("Active Spectrum2 first!.");
    msgBox.exec();
    }
}

void spectra::on_buttonSpectrum3_clicked()
{
    if ( ui->textSpectrum3->isEnabled() ) {
        QString directory = QFileDialog::getOpenFileName(this, tr("Find Files"), QDir::currentPath());
        ui->textSpectrum3->setText(directory);
    } else {
    QMessageBox msgBox;
    msgBox.setText("Active Spectrum3 first!.");
    msgBox.exec();
    }
}

void spectra::on_buttonBox_accepted()
{
    const QString SS_DIR = QDir::currentPath()+QDir::separator();
    int lunt1 = 0;
    int lunt2 = 0;
    int lunt3 = 0;
    QString ins1_txt, ins2_txt, ins3_txt;

    if (ui->checkSpectrum1->isChecked())
    {
        QString path_to_spec1 = ui->textSpectrum1->text();
        QFileInfo check_file(path_to_spec1);
        if (check_file.exists() && check_file.isFile())
        {
            lunt1 = 1;
            ins1_txt = path_to_spec1;
            QFile::copy(path_to_spec1, SS_DIR + "starspec1");
        } else {
            lunt1 = 0;
            ins1_txt = "";
            QMessageBox msgbox;
            msgbox.warning(this, "Spectrum1 not found.", "Path to spectrum1 not found. A Blackbody will be used.");
        }
    }
    else
    {
        lunt1 = 0;
        ins1_txt = "";
    }

    if (ui->checkSpectrum2->isChecked())
    {
        QString path_to_spec2 = ui->textSpectrum2->text();
        QFileInfo check_file(path_to_spec2);
        if (check_file.exists() && check_file.isFile())
        {
            lunt2 = 1;
            ins2_txt = path_to_spec2;
            QFile::copy(path_to_spec2, SS_DIR + "starspec2");
        } else {
            lunt2 = 0;
            ins2_txt = "";
            QMessageBox msgbox;
            msgbox.warning(this, "Spectrum2 not found.", "Path to spectrum2 not found. A Blackbody will be used.");
        }
    }
    else
    {
        lunt2 = 0;
        ins2_txt = "";
    }

    if (ui->checkSpectrum3->isChecked())
    {
        QString path_to_spec3 = ui->textSpectrum3->text();
        QFileInfo check_file(path_to_spec3);
        if (check_file.exists() && check_file.isFile())
        {
            lunt3 = 1;
            ins3_txt = path_to_spec3;
            QFile::copy(path_to_spec3, SS_DIR + "starspec3");
        } else {
            lunt3 = 0;
            ins3_txt = "";
            QMessageBox msgbox;
            msgbox.warning(this, "Spectrum3 not found.", "Path to spectrum3 not found. A Blackbody will be used.");
        }
    }
    else
    {
        lunt3 = 0;
        ins3_txt = "";
    }

    // Adding things to the DB.
    conn.connOpen();
    QSqlQuery qry;
    qry.prepare("UPDATE user_inputs SET lunt1=:lunt1, lunt2=:lunt2, lunt3=:lunt3,"
                " ins1_txt=:ins1_txt, ins2_txt=:ins2_txt, ins3_txt=:ins3_txt WHERE ID=2");
    qry.bindValue(":lunt1", lunt1);
    qry.bindValue(":lunt2", lunt2);
    qry.bindValue(":lunt3", lunt3);
    qry.bindValue(":ins1_txt", ins1_txt);
    qry.bindValue(":ins2_txt", ins2_txt);
    qry.bindValue(":ins3_txt", ins3_txt);

    if (qry.exec())
    {
        qDebug()<<"Spectra DB:"<<"Executed correctly."<< endl;
        conn.connClose();
    }
    else
    {
        qDebug()<<"Spectra DB error:"<<qry.lastError().text()<<endl;
        qDebug()<<qry.executedQuery()<<endl;
        qDebug()<<qry.lastQuery()<<endl;
        conn.connClose();
    }

}

void spectra::set_default_user_inputs(QString field){
    QString id = "1";
    if (field == "user"){
        id = "2";
    } else if (field == "default"){
        id = "1";
    }
    shellspec conn;
    conn.connOpen();
    QSqlQueryModel qry;
    qry.setQuery("SELECT lunt1, lunt2, lunt3, ins1_txt, ins2_txt, ins3_txt"
                " FROM user_inputs WHERE ID="+id);
    qDebug()<<"Spectra defaults DB errors: "<<qry.lastError()<<endl;
    if (qry.record(0).value("lunt1").toInt() == 1){
        ui->checkSpectrum1->setChecked(true);
    } else {
        ui->checkSpectrum1->setChecked(false);
    }
    if (qry.record(0).value("lunt2").toInt() == 1){
        ui->checkSpectrum2->setChecked(true);
    } else {
        ui->checkSpectrum2->setChecked(false);
    }
    if (qry.record(0).value("lunt3").toInt() == 1){
        ui->checkSpectrum3->setChecked(true);
    } else {
        ui->checkSpectrum3->setChecked(false);
    }
    ui->textSpectrum1->setText(qry.record(0).value("ins1_txt").toString());
    ui->textSpectrum2->setText(qry.record(0).value("ins2_txt").toString());
    ui->textSpectrum3->setText(qry.record(0).value("ins3_txt").toString());

    conn.connClose();
}
