package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.ItemDTO;
import com.proyect.masterdata.dto.OrderDTO;
import com.proyect.masterdata.dto.request.RequestCustomer;
import com.proyect.masterdata.dto.request.RequestItem;
import com.proyect.masterdata.dto.request.RequestOrderSave;
import com.proyect.masterdata.dto.request.RequestSale;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.ICustomer;
import com.proyect.masterdata.services.IItem;
import com.proyect.masterdata.services.IOrdering;
import com.proyect.masterdata.services.ISale;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.List;

@RequiredArgsConstructor
@Service
@Log4j2
public class OrderingImpl implements IOrdering {

    private final UserRepository userRepository;
    private final OrderingRepository orderingRepository;
    private final OrderStateRepository orderStateRepository;
    private final ISale iSale;
    private final ICustomer iCustomer;
    private final IItem iItem;
    private final OrderingRepositoryCustom orderingRepositoryCustom;
    private final SaleRepository saleRepository;
    private final CustomerRepository customerRepository;
    private final ItemRepository itemRepository;
    @Override
    public ResponseSuccess save(RequestOrderSave requestOrderSave, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        OrderState orderState;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            orderState = orderStateRepository.findByNameAndStatusTrue("PENDIENTE");
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        try{

            Ordering ordering = orderingRepository.save(Ordering.builder()
                            .cancellation(false)
                            .orderState(orderState)
                            .orderStateId(orderState.getId())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .deliveryMan(requestOrderSave.getDeliveryMan())
                            .deliveryPhone(requestOrderSave.getDeliveryManPhone())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .tokenUser(user.getUsername())
                    .build());

            RequestSale requestSale = RequestSale.builder()
                    .saleChannel(requestOrderSave.getSaleChannel())
                    .seller(requestOrderSave.getSeller())
                    .paymentReceipt(requestOrderSave.getPaymentReceipt())
                    .paymentMethod(requestOrderSave.getPaymentMethod())
                    .observations(requestOrderSave.getObservations())
                    .managementType(requestOrderSave.getManagementType())
                    .deliveryAmount(requestOrderSave.getDeliveryAmount())
                    .deliveryAddress(requestOrderSave.getDeliveryAddress())
                    .saleAmount(requestOrderSave.getSaleAmount())
                    .advancedPayment(requestOrderSave.getAdvancedPayment())
                    .build();

            iSale.save(ordering,requestSale,tokenUser);

            RequestCustomer requestCustomer = RequestCustomer.builder()
                    .phone(requestOrderSave.getCustomerPhone())
                    .name(requestOrderSave.getCustomerName())
                    .type(requestOrderSave.getCustomerType())
                    .district(requestOrderSave.getCustomerDistrict())
                    .province(requestOrderSave.getCustomerProvince())
                    .department(requestOrderSave.getCustomerDepartment())
                    .instagram(requestOrderSave.getInstagram())
                    .reference(requestOrderSave.getCustomerReference())
                    .address(requestOrderSave.getCustomerAddress())
                    .build();

            iCustomer.save(ordering,requestCustomer,tokenUser);

            for(RequestItem requestItem : requestOrderSave.getRequestItems()){
                iItem.save(ordering,requestItem,tokenUser);
            }

            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();

        }catch (RuntimeException e){
            e.printStackTrace();
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public Page<OrderDTO> list(Long id,String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {

        Page<Ordering> pageOrdering;
        User userdata;

        if (user != null) {
            userdata = userRepository.findByUsernameAndStatusTrue(user.toUpperCase());
        } else {
            userdata = null;
        }

        try{
            pageOrdering = orderingRepositoryCustom.searchForOrdering(id,userdata,sort,sortColumn,pageNumber,pageSize);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if(pageOrdering.isEmpty()){
            return new PageImpl<>(Collections.emptyList());
        }

        List<OrderDTO> orderDTOS = pageOrdering.getContent().stream().map(order -> {
            Sale sale = saleRepository.findByOrderId(order.getId());
            Customer customer = customerRepository.findByOrderId(order.getId());

            List<ItemDTO> itemDTOS = itemRepository.findAllByOrderId(order.getId()).stream().map(item -> ItemDTO.builder()
                    .product(item.getProduct())
                    .quantity(item.getQuantity())
                    .build()).toList();

            return OrderDTO.builder()
                    .id(order.getId())
                    .advancedPayment(sale.getAdvancePayment())
                    .customerName(customer.getName())
                    .customerPhone(customer.getPhone())
                    .customerType(customer.getType())
                    .deliveryPhone(order.getDeliveryPhone())
                    .deliveryMan(order.getDeliveryMan())
                    .orderStatus(order.getOrderState().getName())
                    .department(customer.getDepartment().getName())
                    .province(customer.getProvince().getName())
                    .district(customer.getDistrict().getName())
                    .address(customer.getAddress())
                    .instagram(customer.getInstagram())
                    .saleAmount(sale.getSaleAmount())
                    .deliveryAmount(sale.getSaleAmount())
                    .advancedPayment(sale.getAdvancePayment())
                    .managementType(sale.getManagementType().getName())
                    .reference(customer.getReference())
                    .duePayment((sale.getSaleAmount()+sale.getDeliveryAmount())-sale.getAdvancePayment())
                    .saleChannel(sale.getSaleChannel().getName())
                    .paymentReceipt(sale.getPaymentReceipt())
                    .sellerName(sale.getSeller())
                    .registrationDate(order.getRegistrationDate())
                    .paymentType(sale.getPaymentMethod().getName())
                    .deliveryAddress(sale.getDeliveryAddress())
                    .items(itemDTOS)
                    .build();
        }).toList();

        return new PageImpl<>(orderDTOS,pageOrdering.getPageable(),pageOrdering.getTotalElements());
    }
}
