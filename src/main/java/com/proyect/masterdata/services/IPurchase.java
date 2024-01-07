package com.proyect.masterdata.services;

import java.util.List;

import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.PurchaseDTO;
import com.proyect.masterdata.dto.request.RequestPurchase;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IPurchase {

    public ResponseSuccess save(String serial, List<RequestPurchase> items, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;

    Page<PurchaseDTO> list(String serial, String user, String sort, String sortColumn,
            Integer pageNumber, Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions;
}
