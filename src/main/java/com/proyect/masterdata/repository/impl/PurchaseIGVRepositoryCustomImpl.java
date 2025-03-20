package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.PurchaseIGV;
import com.proyect.masterdata.repository.PurchaseIGVRepositoryCustom;
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
public class PurchaseIGVRepositoryCustomImpl implements PurchaseIGVRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<PurchaseIGV> searchForPurchaseIGV(
            UUID clientId,
            String name,
            Double value,
            Boolean percentage,
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
        CriteriaQuery<PurchaseIGV> criteriaQuery = criteriaBuilder.createQuery(PurchaseIGV.class);

        Root<PurchaseIGV> itemRoot = criteriaQuery.from(PurchaseIGV.class);
        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(
                clientId,
                name,
                value,
                percentage,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status,
                criteriaBuilder,
                itemRoot
        );
        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> purchaseDiscountList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                purchaseDiscountList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                purchaseDiscountList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(purchaseDiscountList);

        } else {

            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }
        TypedQuery<PurchaseIGV> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getPurchaseIGVCount(
                clientId,
                name,
                value,
                percentage,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status
        );
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }
    private List<Predicate> predicateConditions(
            UUID clientId,
            String name,
            Double value,
            Boolean percentage,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<PurchaseIGV> itemRoot
    ){
        List<Predicate> conditions = new ArrayList<>();

        if(name != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(itemRoot.get("name")),"%"+name.toUpperCase()+"%"));
        }

        if(value != null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("value"), value)));
        }

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
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

        if(Boolean.TRUE.equals(percentage)) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("percentage"))));
        }

        if(Boolean.FALSE.equals(percentage)) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("percentage"))));
        }

        return conditions;
    }
    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<PurchaseIGV> itemRoot) {

        List<Order> purchaseDiscountList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("name")) {
            purchaseDiscountList.add(criteriaBuilder.asc(itemRoot.get("name")));
        }

        if (sortColumn.equalsIgnoreCase("clientId")) {
            purchaseDiscountList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        return purchaseDiscountList;
    }
    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<PurchaseIGV> itemRoot) {

        List<Order> purchaseDiscountList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("name")) {
            purchaseDiscountList.add(criteriaBuilder.asc(itemRoot.get("name")));
        }
        
        if (sortColumn.equalsIgnoreCase("clientId")) {
            purchaseDiscountList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        return purchaseDiscountList;
    }
    private Long getPurchaseIGVCount(
            UUID clientId,
            String name,
            Double value,
            Boolean percentage,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            Boolean status
    ){
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<PurchaseIGV> itemRoot = criteriaQuery.from(PurchaseIGV.class);
        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(
                clientId,
                name,
                value,
                percentage,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status,
                criteriaBuilder,
                itemRoot
        );
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
