/*
 *  Copyright (C) 2004 Steve Harris
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation; either version 2.1
 *  of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  $Id: lo_throw.h,v 1.2 2009/02/24 17:13:09 rbd Exp $
 */

#ifndef LO_THROW_H
#define LO_THROW_H

#ifdef __cplusplus
extern "C" {
#endif

void lo_throw(lo_server s, int errnum, const char *message, const char *path);

#ifdef __cplusplus
}
#endif

#endif
