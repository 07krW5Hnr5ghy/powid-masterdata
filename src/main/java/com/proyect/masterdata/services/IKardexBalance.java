package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.KardexBalance;
import com.proyect.masterdata.dto.request.RequestKardexBalance;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IKardexBalance {
    KardexBalance save(RequestKardexBalance requestKardexBalance) throws BadRequestExceptions, InternalErrorExceptions;
}
