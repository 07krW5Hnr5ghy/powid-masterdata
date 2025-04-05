package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.KardexOutput;
import com.proyect.masterdata.dto.request.RequestKardexOutput;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IkardexOutput {
    KardexOutput save(RequestKardexOutput requestKardexOutput) throws BadRequestExceptions, InternalErrorExceptions;
}
