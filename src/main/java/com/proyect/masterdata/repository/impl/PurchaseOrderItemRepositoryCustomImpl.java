package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.repository.PurchaseOrderItemRepositoryCustom;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Repository
public class PurchaseOrderItemRepositoryCustomImpl implements PurchaseOrderItemRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
    public Page<PurchaseOrderItem> searchForPurchaseOrderItem(
            UUID clientId,
            Long orderNumber,
            String ref,
            Integer quantity,
            String model,
            String product,
            String color,
            String size,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<PurchaseOrderItem> criteriaQuery = criteriaBuilder.createQuery(PurchaseOrderItem.class);
        Root<PurchaseOrderItem> itemRoot = criteriaQuery.from(PurchaseOrderItem.class);
        Join<PurchaseOrderItem, PurchaseOrder> purchaseOrderItemPurchaseOrderJoin = itemRoot.join("purchaseOrder");
        Join<PurchaseOrderItem, Product> purchaseOrderItemProductJoin = itemRoot.join("product");
        Join<Product, Model> productModelJoin = purchaseOrderItemProductJoin.join("model");
        Join<Product,Color> productColorJoin = purchaseOrderItemProductJoin.join("color");
        Join<Product,Size> productSizeJoin = purchaseOrderItemProductJoin.join("size");

        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicate(
                clientId,
                orderNumber,
                ref,
                quantity,
                model,
                product,
                color,
                size,
                status,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                criteriaBuilder,
                itemRoot,
                purchaseOrderItemPurchaseOrderJoin,
                purchaseOrderItemProductJoin,
                productModelJoin,
                productColorJoin,
                productSizeJoin);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> purchaseItemList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                purchaseItemList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                purchaseItemList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(purchaseItemList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<PurchaseOrderItem> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getPurchaseOrderItemCount(
                clientId,
                orderNumber,
                ref,
                quantity,
                model,
                product,
                color,
                size,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }

    private List<Predicate> predicate(
            UUID clientId,
            Long orderNumber,
            String ref,
            Integer quantity,
            String model,
            String product,
            String color,
            String size,
            Boolean status,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            CriteriaBuilder criteriaBuilder,
            Root<PurchaseOrderItem> itemRoot,
            Join<PurchaseOrderItem, PurchaseOrder> purchaseOrderItemPurchaseOrderJoin,
            Join<PurchaseOrderItem, Product> purchaseOrderItemProductJoin,
            Join<Product, Model> productModelJoin,
            Join<Product,Color> productColorJoin,
            Join<Product,Size> productSizeJoin
    ) {

        List<Predicate> conditions = new ArrayList<>();
        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }
        if(orderNumber!=null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(purchaseOrderItemPurchaseOrderJoin.get("orderNumber"), orderNumber)));
        }
        if(ref != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(purchaseOrderItemPurchaseOrderJoin.get("ref")),"%"+ref.toUpperCase()+"%"));
        }

        if(quantity!=null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("quantity"), quantity)));
        }

        if(model!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(productModelJoin.get("name")),"%"+model.toUpperCase()+"%"));
        }

        if(product!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(purchaseOrderItemProductJoin.get("name")),"%"+product.toUpperCase()+"%"));
        }

        if(color!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(productColorJoin.get("name")),"%"+color.toUpperCase()+"%"));
        }

        if(size!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(productSizeJoin.get("name")),"%"+size.toUpperCase()+"%"));
        }

        if(registrationStartDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.greaterThanOrEqualTo(itemRoot.get("registrationDate"),registrationStartDate)
                    )
            );
        }

        if(registrationEndDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.lessThanOrEqualTo(itemRoot.get("registrationDate"),registrationEndDate)
                    )
            );
        }

        if(updateStartDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.greaterThanOrEqualTo(itemRoot.get("updateDate"),updateStartDate)
                    )
            );
        }

        if(updateEndDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.lessThanOrEqualTo(itemRoot.get("updateDate"),updateEndDate)
                    )
            );
        }

        if(Boolean.TRUE.equals(status)) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if(Boolean.FALSE.equals(status)) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<PurchaseOrderItem> itemRoot) {

        List<Order> purchaseOrderItemList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("orderNumber")) {
            purchaseOrderItemList.add(criteriaBuilder.asc(itemRoot.get("orderNumber")));
        }

        if (sortColumn.equalsIgnoreCase("clientId")) {
            purchaseOrderItemList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if(sortColumn.equalsIgnoreCase("updateDate")){
            purchaseOrderItemList.add(criteriaBuilder.asc(itemRoot.get(
                    "updateDate"
            )));
        }

        if(sortColumn.equalsIgnoreCase("registrationDate")){
            purchaseOrderItemList.add(criteriaBuilder.asc(itemRoot.get(
                    "registrationDate"
            )));
        }

        return purchaseOrderItemList;

    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<PurchaseOrderItem> itemRoot) {

        List<Order> purchaseOrderItemList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("orderNumber")) {
            purchaseOrderItemList.add(criteriaBuilder.asc(itemRoot.get("orderNumber")));
        }

        if (sortColumn.equalsIgnoreCase("name")) {
            purchaseOrderItemList.add(criteriaBuilder.desc(itemRoot.get("name")));
        }

        if (sortColumn.equalsIgnoreCase("clientId")) {
            purchaseOrderItemList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if(sortColumn.equalsIgnoreCase("updateDate")){
            purchaseOrderItemList.add(criteriaBuilder.asc(itemRoot.get(
                    "updateDate"
            )));
        }

        if(sortColumn.equalsIgnoreCase("registrationDate")){
            purchaseOrderItemList.add(criteriaBuilder.asc(itemRoot.get(
                    "registrationDate"
            )));
        }

        return purchaseOrderItemList;

    }

    private Long getPurchaseOrderItemCount(
            UUID clientId,
            Long orderNumber,
            String ref,
            Integer quantity,
            String model,
            String product,
            String color,
            String size,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<PurchaseOrderItem> itemRoot = criteriaQuery.from(PurchaseOrderItem.class);
        Join<PurchaseOrderItem, PurchaseOrder> purchaseOrderItemPurchaseOrderJoin = itemRoot.join("purchaseOrder");
        Join<PurchaseOrderItem, Product> purchaseOrderItemProductJoin = itemRoot.join("product");
        Join<Product, Model> productModelJoin = purchaseOrderItemProductJoin.join("model");
        Join<Product,Color> productColorJoin = purchaseOrderItemProductJoin.join("color");
        Join<Product,Size> productSizeJoin = purchaseOrderItemProductJoin.join("size");
        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(
                clientId,
                orderNumber,
                ref,
                quantity,
                model,
                product,
                color,
                size,
                status,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                criteriaBuilder,
                itemRoot,
                purchaseOrderItemPurchaseOrderJoin,
                purchaseOrderItemProductJoin,
                productModelJoin,
                productColorJoin,
                productSizeJoin);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
