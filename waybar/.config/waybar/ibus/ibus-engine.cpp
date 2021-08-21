#include <ibus.h>
#include <iostream>
#include <string>

const std::string japanese_json = R"({"text":"日本語"})";
std::string japanese_engine = "";

gboolean global_engine_changed_cb(IBusBus *bus, gchar *name_c, gpointer user_data)
{
    std::string name(name_c);
    if (name == japanese_engine) {
        std::cout << japanese_json << std::endl;
    } else {
        std::cout << std::endl;
    }
    return true;
}

int main(int argc, char** argv)
{
    if (argc < 2) {
        std::cerr << "No japanese engine provided" << std::endl;
        return 1;
    } else {
        japanese_engine = std::string(argv[1]);
    }

    ibus_init();

    IBusBus* bus = ibus_bus_new();

    ibus_bus_set_watch_ibus_signal(bus, true);

    g_signal_connect(bus, "global-engine-changed",
                     G_CALLBACK(global_engine_changed_cb), bus);

    ibus_main();
}
