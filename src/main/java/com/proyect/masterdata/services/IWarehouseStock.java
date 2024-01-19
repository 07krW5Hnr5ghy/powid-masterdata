package com.proyect.masterdata.services;

import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.WarehouseStockDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IWarehouseStock {
        public ResponseSuccess in(String warehouse, String supplierProductSerial, Integer quantity, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;

        public ResponseSuccess out(String warehouse, String supplierProductSerial, Integer quantity, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;

        public Page<WarehouseStockDTO> list(String warehouse, String user, String sort, String sortColumn,
                        Integer pageNumber,
                        Integer pageSize) throws InternalErrorExceptions;
}
