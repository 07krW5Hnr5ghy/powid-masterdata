package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.repository.KardexBalanceRepositoryCustom;
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
public class KardexBalanceRepositoryCustomImpl implements KardexBalanceRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<KardexBalance> searchForKardexBalance(UUID clientId, Integer quantity, Long lotNumber, String product, UUID productId, String username, String warehouse, Double unitPrice, OffsetDateTime registrationStartDate, OffsetDateTime registrationEndDate, OffsetDateTime updateStartDate, OffsetDateTime updateEndDate, String sort, String sortColumn, Integer pageNumber, Integer pageSize) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<KardexBalance> criteriaQuery = criteriaBuilder.createQuery(KardexBalance.class);
        Root<KardexBalance> itemRoot = criteriaQuery.from(KardexBalance.class);
        Join<KardexBalance, Product> kardexBalanceProductJoin = itemRoot.join("product");
        Join<KardexBalance, Warehouse> kardexBalanceWarehouseJoin = itemRoot.join("warehouse");
        Join<KardexBalance, User> kardexBalanceUserJoin = itemRoot.join("user");
        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicate(
                clientId,
                quantity,
                lotNumber,
                product,
                productId,
                username,
                warehouse,
                unitPrice,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                criteriaBuilder,
                itemRoot,
                kardexBalanceProductJoin,
                kardexBalanceUserJoin,
                kardexBalanceWarehouseJoin);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> kardexBalanceList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                kardexBalanceList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                kardexBalanceList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(kardexBalanceList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<KardexBalance> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(
                clientId,
                quantity,
                lotNumber,
                product,
                productId,
                username,
                warehouse,
                unitPrice,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate
        );

        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }
    private List<Predicate> predicate(
            UUID clientId,
            Integer quantity,
            Long lotNumber,
            String product,
            UUID productId,
            String username,
            String warehouse,
            Double unitPrice,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            CriteriaBuilder criteriaBuilder,
            Root<KardexBalance> itemRoot,
            Join<KardexBalance,Product> kardexBalanceProductJoin,
            Join<KardexBalance,User> kardexBalanceUserJoin,
            Join<KardexBalance,Warehouse> kardexBalanceWarehouseJoin) {

        List<Predicate> conditions = new ArrayList<>();

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if (quantity != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("remainingQuantity"), quantity)));
        }

        if (lotNumber != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("lotNumber"), lotNumber)));
        }

        if (productId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("productId"), productId)));
        }

        if(product != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(kardexBalanceProductJoin.get("name")),"%"+product.toUpperCase()+"%"));
        }

        if(product != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(kardexBalanceProductJoin.get("name")),"%"+product.toUpperCase()+"%"));
        }

        if(warehouse != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(kardexBalanceWarehouseJoin.get("name")),"%"+warehouse.toUpperCase()+"%"));
        }

        if(username!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(kardexBalanceUserJoin.get("name")),"%"+username.toUpperCase()+"%"));
        }

        if (unitPrice != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("unitPrice"), unitPrice)));
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

        return conditions;
    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<KardexBalance> itemRoot) {

        List<Order> kardexBalanceList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            kardexBalanceList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("supplierProductId")) {
            kardexBalanceList.add(criteriaBuilder.asc(itemRoot.get("supplierProductId")));
        }

        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            kardexBalanceList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            kardexBalanceList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            kardexBalanceList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            kardexBalanceList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        return kardexBalanceList;

    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<KardexBalance> itemRoot) {

        List<Order> kardexBalanceList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            kardexBalanceList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("supplierProductId")) {
            kardexBalanceList.add(criteriaBuilder.desc(itemRoot.get("supplierProductId")));
        }

        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            kardexBalanceList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            kardexBalanceList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            kardexBalanceList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            kardexBalanceList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }

        return kardexBalanceList;

    }

    private Long getOrderCount(
            UUID clientId,
            Integer quantity,
            Long lotNumber,
            String product,
            UUID productId,
            String username,
            String warehouse,
            Double unitPrice,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate
    ) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<KardexBalance> itemRoot = criteriaQuery.from(KardexBalance.class);
        Join<KardexBalance,Product> kardexBalanceProductJoin = itemRoot.join("product");
        Join<KardexBalance,Warehouse> kardexBalanceWarehouseJoin = itemRoot.join("warehouse");
        Join<KardexBalance,User> kardexBalanceUserJoin = itemRoot.join("user");
        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(
                clientId,
                quantity,
                lotNumber,
                product,
                productId,
                username,
                warehouse,
                unitPrice,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                criteriaBuilder,
                itemRoot,
                kardexBalanceProductJoin,
                kardexBalanceUserJoin,
                kardexBalanceWarehouseJoin);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
