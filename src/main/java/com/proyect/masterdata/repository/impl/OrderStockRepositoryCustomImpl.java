package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.OrderStock;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.Warehouse;
import com.proyect.masterdata.repository.OrderStockRepositoryCustom;
import io.micrometer.common.util.StringUtils;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.*;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;

@Repository
public class OrderStockRepositoryCustomImpl implements OrderStockRepositoryCustom {

    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
    public Page<OrderStock> searchForOrderStock(Warehouse warehouse, Long orderId, User user, Boolean status, String sort, String sortColumn, Integer pageNumber, Integer pageSize) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<OrderStock> criteriaQuery = criteriaBuilder.createQuery(OrderStock.class);
        Root<OrderStock> itemRoot = criteriaQuery.from(OrderStock.class);

        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(warehouse,orderId,user,status,criteriaBuilder,itemRoot);
        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> orderStockList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                orderStockList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                orderStockList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(orderStockList);

        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<OrderStock> orderingTypedQuery = entityManager.createQuery(criteriaQuery);
        orderingTypedQuery.setFirstResult(pageNumber * pageSize);
        orderingTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber,pageSize);
        Long count = getOrderCount(warehouse,orderId,user,status);
        return new PageImpl<>(orderingTypedQuery.getResultList(),pageable,count);
    }

    List<Predicate> predicateConditions(Warehouse warehouse,Long orderId,User user,Boolean status,CriteriaBuilder criteriaBuilder,Root<OrderStock> itemRoot){
        List<Predicate> conditions = new ArrayList<>();

        if(warehouse != null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("warehouseId"),warehouse.getId())));
        }

        if(orderId != null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("orderId"),orderId)));
        }

        if(user != null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"),user.getClientId())));
        }

        if (status){
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if (!status){
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    private List<Order> listASC(String sortColumn,CriteriaBuilder criteriaBuilder,Root<OrderStock> itemRoot){
        List<Order> orderStockList = new ArrayList<>();

        if(sortColumn.equals("warehouseId")){
            orderStockList.add(criteriaBuilder.asc(itemRoot.get("warehouseId")));
        }

        if(sortColumn.equals("orderId")){
            orderStockList.add(criteriaBuilder.asc(itemRoot.get("orderId")));
        }

        if(sortColumn.equals("clientId")){
            orderStockList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        return orderStockList;
    }

    private List<Order> listDESC(String sortColumn,CriteriaBuilder criteriaBuilder,Root<OrderStock> itemRoot){
        List<Order> orderStockList = new ArrayList<>();

        if(sortColumn.equals("warehouseId")){
            orderStockList.add(criteriaBuilder.desc(itemRoot.get("warehouseId")));
        }

        if(sortColumn.equals("orderId")){
            orderStockList.add(criteriaBuilder.desc(itemRoot.get("orderId")));
        }

        if(sortColumn.equals("clientId")){
            orderStockList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        return orderStockList;
    }

    private Long getOrderCount(Warehouse warehouse,Long orderId,User user,Boolean status){
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<OrderStock> itemRoot = criteriaQuery.from(OrderStock.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(warehouse,orderId,user,status,criteriaBuilder,itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
