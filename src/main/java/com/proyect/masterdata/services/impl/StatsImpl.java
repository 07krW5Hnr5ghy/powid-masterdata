package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.StatsCardDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IStats;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class StatsImpl implements IStats {
    private final UserRepository userRepository;
    private final OrderStateRepository orderStateRepository;
    private final OrderingRepository orderingRepository;
    private final OrderItemRepository orderItemRepository;
    private final ProductPriceRepository productPriceRepository;
    @Override
    public CompletableFuture<StatsCardDTO> listCardStats(
            Date updateStartDate,
            Date updateEndDate,
            String orderStateName,
            String username) throws BadRequestExceptions, InterruptedException {
        return CompletableFuture.supplyAsync(()->{
            User user;
            OrderState orderState;
            List<Ordering> orderingListByDate;
            List<Ordering> orderingListByDateAndStatus;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                orderState = orderStateRepository.findByName(orderStateName);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                orderingListByDate = orderingRepository.findByClientIdAndUpdateDateBetween(user.getClientId(),updateStartDate,updateEndDate);
            }
            if (orderState==null){
                orderingListByDateAndStatus = orderingRepository.findByClientIdAndUpdateDateBetween(user.getClientId(),updateStartDate,updateEndDate);
            }else {
                orderingListByDateAndStatus = orderingRepository.findByClientIdAndUpdateDateBetweenAndOrderStateId(user.getClientId(),updateStartDate,updateEndDate,orderState.getId());
            }
            try{
                Integer totalOrdersByDate = orderingListByDate.size();
                int totalOrdersByDateAndStatus = orderingListByDateAndStatus.size();
                String state;
                if(orderState!=null){
                    state = orderState.getName();
                }else{
                    state = "NO APLICA";
                }
                double totalSales = 0.00;
                Double totalDeliveryAmount = 0.00;
                int totalProducts = 0;
                for(Ordering ordering:orderingListByDateAndStatus){
                    double totalSaleByOrder = 0.00;
                    Integer totalProductsByOrder = 0;
                    List<OrderItem> orderItems = orderItemRepository.findAllByOrderId(ordering.getId());
                    for(OrderItem orderItem:orderItems){
                        ProductPrice productPrice = productPriceRepository.findByProductIdAndStatusTrue(orderItem.getProductId());
                        if(Objects.equals(orderItem.getDiscount().getName(), "PORCENTAJE")) {
                            totalSaleByOrder += (productPrice.getUnitSalePrice() * orderItem.getQuantity()) - ((productPrice.getUnitSalePrice() * orderItem.getQuantity()) * (orderItem.getDiscountAmount() / 100));
                        }
                        if(Objects.equals(orderItem.getDiscount().getName(), "MONTO")){
                            totalSaleByOrder += (productPrice.getUnitSalePrice() * orderItem.getQuantity()) - orderItem.getDiscountAmount();
                        }
                        if(Objects.equals(orderItem.getDiscount().getName(), "NO APLICA")){
                            totalSaleByOrder += (productPrice.getUnitSalePrice() * orderItem.getQuantity());
                        }
                        totalProductsByOrder += orderItem.getQuantity();
                    }
                    totalSales += totalSaleByOrder;
                    totalDeliveryAmount += ordering.getDeliveryAmount();
                    totalProducts += totalProductsByOrder;
                }

                System.out.println(totalSales);
                System.out.println(totalProducts);

                double averageSaleProduct;
                if(totalSales > 0.00 && totalProducts > 0){
                    System.out.println("check");
                    averageSaleProduct = totalSales/totalProducts;
                }else{
                    averageSaleProduct = 0.00;
                }

                return StatsCardDTO.builder()
                        .totalOrders(totalOrdersByDateAndStatus)
                        .totalSales(BigDecimal.valueOf(totalSales).setScale(2, RoundingMode.HALF_EVEN))
                        .orderStatus(state)
                        .totalDeliveryAmountOrders(BigDecimal.valueOf(totalDeliveryAmount).setScale(2, RoundingMode.HALF_EVEN))
                        .totalProducts(totalProducts)
                        .averageSaleProduct(averageSaleProduct)
                        .averageTicket(BigDecimal.valueOf(totalProducts/totalOrdersByDateAndStatus))
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
