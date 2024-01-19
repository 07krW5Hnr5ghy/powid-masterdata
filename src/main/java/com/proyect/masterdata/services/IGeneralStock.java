package com.proyect.masterdata.services;

import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.GeneralStockDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IGeneralStock {
        public ResponseSuccess in(String supplierProductSerial, Integer quantity, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;

        public ResponseSuccess out(String supplierProductSerial, Integer quantity, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;

        public Page<GeneralStockDTO> list(String user, String sort, String sortColumn,
                        Integer pageNumber,
                        Integer pageSize) throws InternalErrorExceptions;
}
