package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.Payment;
import com.proyect.masterdata.dto.PaymentDTO;
import com.proyect.masterdata.dto.PaymentUpdateDTO;
import com.proyect.masterdata.dto.request.RequestPaymentSave;
import com.proyect.masterdata.dto.request.RequestPaymentUpdate;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IPayment {
    ResponseSuccess save(RequestPaymentSave requestPaymentSave,String user) throws InternalErrorExceptions, BadRequestExceptions;
    ResponseSuccess saveAll(List<RequestPaymentSave> requestPaymentSaveList,String user) throws InternalErrorExceptions, BadRequestExceptions;
    PaymentUpdateDTO update(RequestPaymentUpdate requestPaymentUpdate, String newPaymentState, String user) throws InternalErrorExceptions,BadRequestExceptions;
    Page<PaymentDTO> list(Double totalPayment, String month, String channel, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
}
