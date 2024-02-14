package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.PurchaseDTO;
import com.proyect.masterdata.dto.request.RequestPurchaseItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IPurchase {
    public ResponseSuccess save(String serial, List<RequestPurchaseItem> requestPurchaseItemList,String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    public Page<PurchaseDTO> list(String serial,String user,String sort,String sortColumn,Integer pageNumber,Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions;
}
