#ifndef SETOPACITIES_H
#define SETOPACITIES_H

#include <QDialog>
#include <shellspec.h>

namespace Ui {
class setOpacities;
}

class setOpacities : public QDialog
{
    Q_OBJECT

public:
    void set_default_user_inputs(QString field);
    explicit setOpacities(QWidget *parent = nullptr);
    ~setOpacities();

private slots:
    void on_buttonLineOP_clicked();
    void on_buttonMS_clicked();
    void on_buttonOpacities_accepted();

private:
    Ui::setOpacities *ui;
};

#endif // SETOPACITIES_H
