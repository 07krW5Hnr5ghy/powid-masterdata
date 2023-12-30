package com.proyect.masterdata.services;

import java.util.List;

import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.ProductDTO;
import com.proyect.masterdata.dto.request.RequestProductSave;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IProduct {

        ResponseSuccess save(RequestProductSave product, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;

        ResponseSuccess saveAll(List<RequestProductSave> products, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;

        ResponseDelete delete(String sku, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;

        Page<ProductDTO> list(String sku, String model, String sort, String sortColumn, Integer pageNumber,
                        Integer pageSize) throws BadRequestExceptions;
}
