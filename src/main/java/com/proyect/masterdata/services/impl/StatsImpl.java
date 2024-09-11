package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.DailySaleSummaryDTO;
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
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

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
            String username) throws BadRequestExceptions, InternalErrorExceptions {
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
                orderingListByDateAndStatus = orderingRepository.findByUpdateDateBetween(updateStartDate,updateEndDate);
            }else {
                orderingListByDateAndStatus = orderingRepository.findByClientIdAndUpdateDateBetweenAndOrderStateId(user.getClientId(),updateStartDate,updateEndDate,orderState.getId());
            }
            try{
                int totalOrdersByDate;
                int totalOrdersByDateAndStatus = orderingListByDateAndStatus.size();
                if(orderingListByDate.isEmpty()){
                    totalOrdersByDate = 0;
                }else{
                    totalOrdersByDate = orderingListByDate.size();
                }
                String state;
                if(orderState!=null){
                    state = orderState.getName();
                }else{
                    state = "TODOS";
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

                if(totalSales <= 0.00 && totalProducts < 1){
                    return StatsCardDTO.builder()
                            .totalOrders(totalOrdersByDateAndStatus)
                            .totalSales(BigDecimal.valueOf(totalSales).setScale(2, RoundingMode.HALF_EVEN))
                            .orderStatus(state)
                            .totalDeliveryAmountOrders(BigDecimal.valueOf(0.00).setScale(2, RoundingMode.HALF_EVEN))
                            .totalProducts(totalProducts)
                            .totalOrdersByRangeDate(0)
                            .averageSaleProduct(BigDecimal.valueOf(0.00).setScale(2,RoundingMode.HALF_EVEN))
                            .averageTicket(BigDecimal.valueOf(0.00).setScale(2, RoundingMode.HALF_EVEN))
                            .percentageOfOrders(BigDecimal.valueOf(0.00).setScale(2, RoundingMode.HALF_EVEN))
                            .build();
                }

                double averageSaleProduct;
                if(totalSales > 0.00 && totalProducts > 0){
                    averageSaleProduct = totalSales/totalProducts;
                }else{
                    averageSaleProduct = 0.00;
                }

                BigDecimal totalOrdersByDateAndStatusBD = BigDecimal.valueOf(totalOrdersByDateAndStatus);
                BigDecimal totalOrdersByDateBD = BigDecimal.valueOf(totalOrdersByDate);

                BigDecimal percentage = totalOrdersByDateAndStatusBD
                        .divide(totalOrdersByDateBD, 2, RoundingMode.HALF_UP) // Divide with 2 decimal places
                        .multiply(BigDecimal.valueOf(100L)).setScale(2,RoundingMode.HALF_EVEN);

                return StatsCardDTO.builder()
                        .totalOrders(totalOrdersByDateAndStatus)
                        .totalOrdersByRangeDate(totalOrdersByDate)
                        .totalSales(BigDecimal.valueOf(totalSales).setScale(2, RoundingMode.HALF_EVEN))
                        .orderStatus(state)
                        .totalDeliveryAmountOrders(BigDecimal.valueOf(totalDeliveryAmount).setScale(2, RoundingMode.HALF_EVEN))
                        .totalProducts(totalProducts)
                        .averageSaleProduct(BigDecimal.valueOf(averageSaleProduct).setScale(2,RoundingMode.HALF_EVEN))
                        .averageTicket(BigDecimal.valueOf(totalProducts/totalOrdersByDateAndStatus).setScale(2,RoundingMode.HALF_EVEN))
                        .percentageOfOrders(percentage)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<DailySaleSummaryDTO>> listDailySales(Date registrationStartDate, Date registrationEndDate, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            List<Ordering> orderingListByDate;
            List<Ordering> orderingListByDateAndStatus;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                orderingListByDate = orderingRepository.findByClientIdAndUpdateDateBetween(user.getClientId(),registrationStartDate,registrationEndDate);
            }
            try{
                int totalOrdersByDate;
                if(orderingListByDate.isEmpty()){
                    totalOrdersByDate = 0;
                }else{
                    totalOrdersByDate = orderingListByDate.size();
                }
                String state;
                double totalSales = 0.00;
                Double totalDeliveryAmount = 0.00;
                int totalProducts = 0;
                List<DailySaleSummaryDTO> dailySaleSummaryDTOS = new ArrayList<>();
                List<DailySaleSummaryDTO> orderDates = orderingRepository.findAllOrdersByDate(
                        user.getClientId(),
                        registrationStartDate,
                        registrationEndDate).stream().map(result -> {
                            return DailySaleSummaryDTO.builder()
                                    .date((Date) result[0])
                                    .totalOrders(((Long) result[1]).intValue())
                                    .build();
                }).toList();
                System.out.println(orderDates);
                return dailySaleSummaryDTOS;
            }catch (RuntimeException e){
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
