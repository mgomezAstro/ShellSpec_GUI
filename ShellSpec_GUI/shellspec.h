#ifndef SHELLSPEC_H
#define SHELLSPEC_H

#include <QMainWindow>
#include <QtSql>
#include <QDir>

extern "C" void shellspec_();

namespace Ui {
class shellspec;
}

class shellspec : public QMainWindow
{
    Q_OBJECT

public:
    QSqlDatabase mydb;
    void connClose(){
        mydb.close();
        mydb.removeDatabase(QSqlDatabase::defaultConnection);
    }
    bool connOpen()
    {
        bool dbrun = false;
        const QString dbDir = QStandardPaths::writableLocation(QStandardPaths::AppConfigLocation);
        // const QString dbDir = QDir::toNativeSeparators(QDir::currentPath()+"/ss_fortran/config_files/config_inputs.db");
        if (!QDir(dbDir).exists())
            QDir().mkdir(dbDir);

        if (QSqlDatabase::isDriverAvailable("QSQLITE"))
        {
            QSqlDatabase mydb = QSqlDatabase::addDatabase("QSQLITE");
            mydb.setDatabaseName(dbDir + QDir::separator() + "config_inputs.db");
//            qDebug()<<"QSQLITE for DB exists!."<<endl;
            if (!mydb.open()){
//                qDebug()<<("Failure in the DB conection.");
                dbrun = false;
            } else {
//                qDebug()<<("Connection to the DB succesuffly.");
                dbrun = true;
            }
        }
        return dbrun;
    }

public:
    explicit shellspec(QWidget *parent = nullptr);
    ~shellspec();
    QString get_prefix_model();
    void rename_all_output_files();
    int count_total_graphs_current_model();

private slots:
    void on_actionInput_Spectra_triggered();
    void on_actionOpacity_triggered();
    void on_actionBody_Frozen_triggered();
    void on_actionLine_of_Sight_triggered();
    void on_actionRun_triggered();
    void on_actionPlot_triggered();
    void on_actionLoad_in_file_triggered();
    void on_actionUser_manual_triggered();
    void on_checkielnd_toggled(bool checked);
    void on_groupCSmain_toggled(bool arg1);
    bool save_all_user_inputs();
    void on_groupCompanion_toggled(bool arg1);
    void on_groupEnvelope_toggled(bool arg1);
    void on_groupSpot_toggled(bool arg1);
    void on_groupStream_toggled(bool arg1);
    void on_groupRing_toggled(bool arg1);
    void on_groupDisc_toggled(bool arg1);
    void on_groupNebula_toggled(bool arg1);
    void on_groupFlow_toggled(bool arg1);
    void on_groupJet_toggled(bool arg1);
    void on_groupUfo_toggled(bool arg1);
    void on_groupShell_toggled(bool arg1);
    void set_defaults_main(QString field);
    void on_buttonAbundances_clicked();
    bool create_table_only_once();


    void on_actionVersion_triggered();

private:
    Ui::shellspec *ui;
};

#endif // SHELLSPEC_H
