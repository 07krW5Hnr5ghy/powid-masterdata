package com.proyect.masterdata.services;

import java.util.List;

import com.proyect.masterdata.dto.response.ResponseDelete;
import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.PurchaseItemDTO;
import com.proyect.masterdata.dto.request.RequestPurchaseItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IPurchaseItem {

    public ResponseSuccess save(Long purchaseId, RequestPurchaseItem requestPurchaseItem, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;

    Page<PurchaseItemDTO> list(String serial, String user, String supplierProductSerial, String sort, String sortColumn,
                               Integer pageNumber, Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions;

    public ResponseDelete delete(String purchaseSerial,String serialSupplierProduct,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
}
