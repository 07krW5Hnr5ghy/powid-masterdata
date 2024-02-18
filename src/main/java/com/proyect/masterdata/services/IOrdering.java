package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.OrderDTO;
import com.proyect.masterdata.dto.request.RequestOrderSave;
import com.proyect.masterdata.dto.request.RequestOrderUpdate;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

public interface IOrdering {
    ResponseSuccess save(RequestOrderSave requestOrderSave, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    Page<OrderDTO> list(Long orderId,String user,String orderState,String courier,String paymentState,String paymentMethod,String saleChannel,String managementType,String sort,String sortColumn,Integer pageNumber,Integer pageSize) throws BadRequestExceptions;
    ResponseSuccess update(Long orderId, RequestOrderUpdate requestOrderUpdate,String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
}
