//
// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//
#include <rtems.h>
#include <stdlib.h>

rtems_task Init(rtems_task_argument argument);
void e4_rtems_main();

rtems_task Init(rtems_task_argument argument) {
    e4_rtems_main();
    exit(0);
}

// configuration information

#include <bsp.h>

// NOTICE: the clock driver is explicitly disabled
#define CONFIGURE_APPLICATION_DOES_NOT_NEED_CLOCK_DRIVER
#define CONFIGURE_APPLICATION_NEEDS_CONSOLE_DRIVER
#define CONFIGURE_USE_DEVFS_AS_BASE_FILESYSTEM

#define CONFIGURE_RTEMS_INIT_TASKS_TABLE
#define CONFIGURE_MAXIMUM_TASKS 1

#define CONFIGURE_INIT
#include <rtems/confdefs.h>
