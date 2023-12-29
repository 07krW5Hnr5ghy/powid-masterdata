package com.proyect.masterdata.services;

import java.util.List;

import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.WarehouseDTO;
import com.proyect.masterdata.dto.request.RequestWarehouse;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IWarehouse {
        public ResponseSuccess save(RequestWarehouse requestWarehouse, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;

        public ResponseSuccess saveAll(List<RequestWarehouse> requestWarehousesList, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;

        public Page<WarehouseDTO> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
                        Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions;
}
