package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.ItemDTO;
import com.proyect.masterdata.dto.OrderDTO;
import com.proyect.masterdata.dto.ProductDTO;
import com.proyect.masterdata.dto.request.*;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.*;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Objects;

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
    private final ProductPriceRepository productPriceRepository;
    private final ProductRepository productRepository;
    private final PaymentMethodRepository paymentMethodRepository;
    private final PaymentStateRepository paymentStateRepository;
    private final CourierRepository courierRepository;
    private final OrderStockRepository orderStockRepository;
    private final IWarehouseStock iWarehouseStock;
    private final IGeneralStock iGeneralStock;
    @Override
    public ResponseSuccess save(RequestOrderSave requestOrderSave, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        OrderState orderState;
        Courier courier;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            orderState = orderStateRepository.findByNameAndStatusTrue("PENDIENTE");
            courier = courierRepository.findByNameAndStatusTrue(requestOrderSave.getCourier().toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(courier == null){
            throw new BadRequestExceptions(Constants.ErrorCourier);
        }

        try{

            Ordering ordering = orderingRepository.save(Ordering.builder()
                            .cancellation(false)
                            .orderState(orderState)
                            .orderStateId(orderState.getId())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .courier(courier)
                            .courierId(courier.getId())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                            .tokenUser(user.getUsername())
                    .build());

            Double saleAmount = 0.00;

            for(RequestItem requestItem : requestOrderSave.getRequestItems()){
                iItem.save(ordering,requestItem,tokenUser);
                Product product = productRepository.findBySkuAndStatusTrue(requestItem.getProductSku().toUpperCase());
                ProductPrice productPrice = productPriceRepository.findByProductId(product.getId());
                saleAmount += (productPrice.getUnitSalePrice() * requestItem.getQuantity());
            }

            RequestSale requestSale = RequestSale.builder()
                    .saleChannel(requestOrderSave.getSaleChannel())
                    .seller(user.getName() + " " + user.getSurname())
                    .paymentReceipt(requestOrderSave.getPaymentReceipt())
                    .paymentMethod(requestOrderSave.getPaymentMethod())
                    .observations(requestOrderSave.getObservations())
                    .managementType(requestOrderSave.getManagementType())
                    .deliveryAmount(requestOrderSave.getDeliveryAmount())
                    .deliveryAddress(requestOrderSave.getDeliveryAddress())
                    .saleAmount(saleAmount)
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
        Long clientId;

        try{
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClient().getId();
            pageOrdering = orderingRepositoryCustom.searchForOrdering(id,clientId,sort,sortColumn,pageNumber,pageSize);
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

            List<ItemDTO> itemDTOS = itemRepository.findAllByOrderId(order.getId()).stream().map(item -> {
                ProductPrice productPrice = productPriceRepository.findByProductId(item.getProductId());
                Double totalPrice = productPrice.getUnitSalePrice() * item.getQuantity();
                return ItemDTO.builder()
                        .id(item.getId())
                        .product(ProductDTO.builder()
                                .sku(item.getProduct().getSku())
                                .model(item.getProduct().getModel().getName())
                                .color(item.getProduct().getColor().getName())
                                .size(item.getProduct().getSize().getName())
                                .category(item.getProduct().getCategoryProduct().getName())
                                .price(productPrice.getUnitSalePrice())
                                .unit(item.getProduct().getUnit().getName())
                                .build())
                        .quantity(item.getQuantity())
                        .unitPrice(productPrice.getUnitSalePrice())
                        .totalPrice(totalPrice)
                        .observations(item.getObservations())
                        .build();
            }).toList();

            return OrderDTO.builder()
                    .id(order.getId())
                    .customerName(customer.getName())
                    .customerPhone(customer.getPhone())
                    .customerType(customer.getType())
                    .orderStatus(order.getOrderState().getName())
                    .department(customer.getDepartment().getName())
                    .province(customer.getProvince().getName())
                    .district(customer.getDistrict().getName())
                    .address(customer.getAddress())
                    .instagram(customer.getInstagram())
                    .deliveryAmount(sale.getSaleAmount())
                    .advancedPayment(sale.getAdvancePayment())
                    .managementType(sale.getManagementType().getName())
                    .reference(customer.getReference())
                    .duePayment((sale.getSaleAmount()+sale.getDeliveryAmount())-sale.getAdvancePayment())
                    .saleChannel(sale.getSaleChannel().getName())
                    .paymentReceipt(sale.getPaymentReceipt())
                    .sellerName(sale.getSeller())
                    .registrationDate(order.getRegistrationDate())
                    .updateDate(order.getUpdateDate())
                    .paymentType(sale.getPaymentMethod().getName())
                    .deliveryAddress(sale.getDeliveryAddress())
                    .courier(order.getCourier().getName())
                    .items(itemDTOS)
                    .saleAmount(sale.getSaleAmount())
                    .build();
        }).toList();

        return new PageImpl<>(orderDTOS,pageOrdering.getPageable(),pageOrdering.getTotalElements());
    }

    @Override
    public ResponseSuccess update(Long orderId, RequestOrderUpdate requestOrderUpdate, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Ordering ordering;
        OrderState orderState;
        Sale sale;
        PaymentMethod paymentMethod;
        PaymentState paymentState;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            ordering = orderingRepository.findById(orderId).orElse(null);
            orderState = orderStateRepository.findByNameAndStatusTrue(requestOrderUpdate.getOrderState().toUpperCase());
            paymentMethod = paymentMethodRepository.findByNameAndStatusTrue(requestOrderUpdate.getPaymentMethod().toUpperCase());
            paymentState = paymentStateRepository.findByNameAndStatusTrue(requestOrderUpdate.getPaymentState().toUpperCase());
        }catch (RuntimeException e){
            e.printStackTrace();
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(ordering == null){
            throw new BadRequestExceptions(Constants.ErrorOrdering);
        }else {
            sale = saleRepository.findByOrderId(ordering.getId());
        }

        try{

            if(!Objects.equals(orderState.getName(), ordering.getOrderState().getName())){
                ordering.setOrderState(orderState);
                ordering.setOrderStateId(orderState.getId());
            }

            if(!Objects.equals(requestOrderUpdate.getObservations(),sale.getObservations()))
            {
                sale.setObservations(requestOrderUpdate.getObservations());
            }

            if(Objects.equals(ordering.getOrderState().getName(), "ENTREGADO")){
                List<Item> orderItems = itemRepository.findAllByOrderId(ordering.getId());
                for(Item item : orderItems){
                    List<OrderStock> orderStockList = orderStockRepository.findByOrderIdAndItemId(ordering.getId(),item.getId());
                    for(OrderStock orderStock : orderStockList){
                        iWarehouseStock.out(orderStock.getWarehouse().getName(),orderStock.getSupplierProduct().getSerial(),orderStock.getQuantity(),user.getUsername());
                        iGeneralStock.out(orderStock.getSupplierProduct().getSerial(),orderStock.getQuantity(),user.getUsername());
                    }
                }
            }

            if(!Objects.equals(paymentMethod.getName(), sale.getPaymentMethod().getName())){
                sale.setPaymentMethod(paymentMethod);
                sale.setPaymentMethodId(paymentMethod.getId());
            }

            if(!Objects.equals(paymentState.getName(),sale.getPaymentState().getName())){
                sale.setPaymentState(paymentState);
                sale.setPaymentMethodId(paymentState.getId());
            }

            orderingRepository.save(ordering);
            saleRepository.save(sale);

            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.update)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }
}
