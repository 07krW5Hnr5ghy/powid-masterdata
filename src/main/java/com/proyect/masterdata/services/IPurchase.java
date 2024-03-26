package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.PurchaseDTO;
import com.proyect.masterdata.dto.request.RequestPurchaseItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IPurchase {
    ResponseSuccess save(String serial,String supplierRuc,String documentName, List<RequestPurchaseItem> requestPurchaseItemList,String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    Page<PurchaseDTO> list(String serial,String user,String documentName,String sort,String sortColumn,Integer pageNumber,Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions;
    List<PurchaseDTO> listPurchase(String user) throws BadRequestExceptions,InternalErrorExceptions;
    List<PurchaseDTO> listPurchaseFalse(String user) throws BadRequestExceptions,InternalErrorExceptions;
}
