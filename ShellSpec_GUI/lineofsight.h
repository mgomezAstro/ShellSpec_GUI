#ifndef LINEOFSIGHT_H
#define LINEOFSIGHT_H

#include <QDialog>
#include "shellspec.h"

namespace Ui {
class lineOfSight;
}

class lineOfSight : public QDialog
{
    Q_OBJECT

public:
    shellspec conn;
    explicit lineOfSight(QWidget *parent = nullptr);
    ~lineOfSight();

private slots:
    void set_default_user_inputs(QString field);
    void on_buttonBox_accepted();

    void on_buttonAddPhases_clicked();

private:
    Ui::lineOfSight *ui;
};

#endif // LINEOFSIGHT_H
