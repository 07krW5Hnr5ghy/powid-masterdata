package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.request.RequestPurchaseItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;

public interface IPurchase {
    public ResponseSuccess save(String serial, List<RequestPurchaseItem> requestPurchaseItemList,String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
}
