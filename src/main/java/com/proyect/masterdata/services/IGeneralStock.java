package com.proyect.masterdata.services;

import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.GeneralStockDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;

public interface IGeneralStock {
        ResponseSuccess in(String supplierProductSerial, Integer quantity, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;
        ResponseSuccess out(String supplierProductSerial, Integer quantity, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;
        Page<GeneralStockDTO> list(String user, String sort, String sortColumn,
                        Integer pageNumber,
                        Integer pageSize) throws InternalErrorExceptions;
        List<GeneralStockDTO> listGeneralStock(String user) throws BadRequestExceptions,InternalErrorExceptions;
}
