package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.Color;
import com.proyect.masterdata.domain.OrderItem;
import com.proyect.masterdata.domain.Product;
import com.proyect.masterdata.repository.OrderItemRepositoryCustom;
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
public class OrderItemRepositoryCustomImpl implements OrderItemRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<OrderItem> searchForOrderItem(
            Long clientId,
            Long orderId,
            String productSku,
            List<Long> colorIds,
            List<Long> sizeIds,
            List<Long> categoryIds,
            Integer quantity,
            Double discount,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<OrderItem> criteriaQuery = criteriaBuilder.createQuery(OrderItem.class);
        Root<OrderItem> itemRoot = criteriaQuery.from(OrderItem.class);
        Join<OrderItem, Product> orderItemProductJoin = itemRoot.join("product");
        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(
                clientId,
                orderId,
                productSku,
                colorIds,
                sizeIds,
                categoryIds,
                quantity,
                discount,
                criteriaBuilder,
                itemRoot,
                orderItemProductJoin,
                status);
        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> orderItemList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                orderItemList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                orderItemList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(orderItemList);

        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<OrderItem> orderingTypedQuery = entityManager.createQuery(criteriaQuery);
        orderingTypedQuery.setFirstResult(pageNumber * pageSize);
        orderingTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber,pageSize);
        Long count = getOrderCount(
                clientId,
                orderId,
                productSku,
                colorIds,
                categoryIds,
                sizeIds,
                quantity,
                discount,
                status);
        return new PageImpl<>(orderingTypedQuery.getResultList(),pageable,count);
    }
    List<Predicate> predicateConditions(
            Long clientId,
            Long orderId,
            String productSku,
            List<Long> colorIds,
            List<Long> sizeIds,
            List<Long> categoryIds,
            Integer quantity,
            Double discount,
            CriteriaBuilder criteriaBuilder,
            Root<OrderItem> itemRoot,
            Join<OrderItem,Product> orderItemProductJoin,
            Boolean status){
        List<Predicate> conditions = new ArrayList<>();

        if(clientId != null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"),clientId)));
        }

        if(orderId != null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("orderId"),orderId)));
        }

        if(productSku != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(orderItemProductJoin.get("sku")),"%"+productSku.toUpperCase()+"%"));
        }

        if(!colorIds.isEmpty()){
            conditions.add(criteriaBuilder.and(orderItemProductJoin.get("colorId").in(colorIds)));
        }

        if(!sizeIds.isEmpty()){
            conditions.add(criteriaBuilder.and(orderItemProductJoin.get("sizeId").in(sizeIds)));
        }

        if(!categoryIds.isEmpty()){
            conditions.add(criteriaBuilder.and(orderItemProductJoin.get("categoryProductId").in(categoryIds)));
        }

        if(quantity != null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("quantity"),quantity)));
        }

        if(discount != null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("discount"),discount)));
        }

        if (status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if (!status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    private List<Order> listASC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<OrderItem> itemRoot){

        List<Order> orderItemList = new ArrayList<>();

        if(sortColumn.equals("clientId")){
            orderItemList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if(sortColumn.equals("orderId")){
            orderItemList.add(criteriaBuilder.asc(itemRoot.get("orderId")));
        }

        if(sortColumn.equals("productId")){
            orderItemList.add(criteriaBuilder.asc(itemRoot.get("productId")));
        }

        if(sortColumn.equals("courierId")){
            orderItemList.add(criteriaBuilder.asc(itemRoot.get("courierId")));
        }

        if(sortColumn.equals("quantity")){
            orderItemList.add(criteriaBuilder.asc(itemRoot.get("quantity")));
        }

        if(sortColumn.equals("discount")){
            orderItemList.add(criteriaBuilder.asc(itemRoot.get("discount")));
        }

        return orderItemList;

    }

    private List<Order> listDESC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<OrderItem> itemRoot){

        List<Order> orderItemList = new ArrayList<>();

        if(sortColumn.equals("clientId")){
            orderItemList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if(sortColumn.equals("orderId")){
            orderItemList.add(criteriaBuilder.desc(itemRoot.get("orderId")));
        }

        if(sortColumn.equals("productId")){
            orderItemList.add(criteriaBuilder.desc(itemRoot.get("productId")));
        }

        if(sortColumn.equals("courierId")){
            orderItemList.add(criteriaBuilder.desc(itemRoot.get("courierId")));
        }

        if(sortColumn.equals("quantity")){
            orderItemList.add(criteriaBuilder.desc(itemRoot.get("quantity")));
        }

        if(sortColumn.equals("discount")){
            orderItemList.add(criteriaBuilder.desc(itemRoot.get("discount")));
        }

        return orderItemList;

    }

    private Long getOrderCount(
            Long clientId,
            Long orderId,
            String productSku,
            List<Long> colorIds,
            List<Long> sizeIds,
            List<Long> categoryIds,
            Integer quantity,
            Double discount,
            Boolean status){
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<OrderItem> itemRoot = criteriaQuery.from(OrderItem.class);
        Join<OrderItem,Product> orderItemProductJoin = itemRoot.join("product");

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(
                clientId,
                orderId,
                productSku,
                colorIds,
                sizeIds,
                categoryIds,
                quantity,
                discount,
                criteriaBuilder,
                itemRoot,
                orderItemProductJoin,
                status);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
