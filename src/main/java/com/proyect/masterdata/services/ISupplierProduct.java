package com.proyect.masterdata.services;

import java.util.List;

import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.SupplierProductDTO;
import com.proyect.masterdata.dto.request.RequestSupplierProduct;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface ISupplierProduct {
        ResponseSuccess save(RequestSupplierProduct requestSupplierProduct, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;
        ResponseSuccess saveAll(List<RequestSupplierProduct> requestSupplierProducts, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;
        ResponseDelete delete(String serial, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
        Page<SupplierProductDTO> list(String serial, String user,String productSku,String supplierRuc, Double purchasePrice, String sort, String sortColumn, Integer pageNumber,
                        Integer pageSize) throws BadRequestExceptions;
        Page<SupplierProductDTO> listFalse(String serial, String user,String productSku,String supplierRuc, Double purchasePrice, String sort, String sortColumn, Integer pageNumber,
                                      Integer pageSize) throws BadRequestExceptions;
}
