package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.PaymentStateDTO;
import com.proyect.masterdata.dto.PaymentTypeDTO;
import com.proyect.masterdata.dto.request.RequestPaymentTypeSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IPaymentType {
    ResponseSuccess save(RequestPaymentTypeSave requestPaymentTypeSave) throws InternalErrorExceptions, BadRequestExceptions;
    ResponseSuccess saveAll(List<String> names,String user) throws InternalErrorExceptions,BadRequestExceptions;
    PaymentTypeDTO update(RequestPaymentTypeSave requestPaymentTypeSave) throws InternalErrorExceptions,BadRequestExceptions;
    ResponseDelete delete(String type, String user) throws InternalErrorExceptions,BadRequestExceptions;
    Page<PaymentTypeDTO> list(String type, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
    Page<PaymentTypeDTO> listStatusFalse(String type,String user,String sort,String sortColumn,Integer pageNumber,Integer pageSize) throws BadRequestExceptions;
}
